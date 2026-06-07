data "google_compute_default_service_account" "default" {
  project = var.project_id
}

locals {
  name_prefix = var.project_name
  common_labels = merge(
    {
      project     = var.project_name
      managed_by  = "terraform"
      application = "openeocraft"
    },
    var.labels
  )
  g2_machine_type = startswith(var.machine_type, "g2-")
  attach_external_gpu = var.enable_gpu && !local.g2_machine_type
  startup_script = templatefile("${path.module}/../shared/cloud_init.tftpl", {
    docker_image        = var.docker_image
    docker_cpus         = var.docker_cpus
    docker_memory_gb    = var.docker_memory_gb
    api_port            = var.api_port
    mount_workspace     = var.workspace_disk_size_gb > 0
    workspace_device    = "/dev/disk/by-id/google-workspace"
    enable_gpu          = var.enable_gpu
    nvidia_driver_major = var.nvidia_driver_major
    swap_size_gb        = var.swap_size_gb
  })
}

resource "google_compute_firewall" "ssh" {
  name    = "${local.name_prefix}-allow-ssh"
  network = var.network_name
  project = var.project_id

  allow {
    protocol = "tcp"
    ports    = ["22"]
  }

  source_ranges = var.ssh_cidr_blocks
  target_tags   = [local.name_prefix]
}

resource "google_compute_firewall" "api" {
  name    = "${local.name_prefix}-allow-api"
  network = var.network_name
  project = var.project_id

  allow {
    protocol = "tcp"
    ports    = [tostring(var.api_port)]
  }

  source_ranges = var.api_cidr_blocks
  target_tags   = [local.name_prefix]
}

resource "google_compute_disk" "workspace" {
  count = var.workspace_disk_size_gb > 0 ? 1 : 0

  name  = "${local.name_prefix}-workspace"
  type  = "pd-balanced"
  zone  = var.zone
  size  = var.workspace_disk_size_gb
  labels = local.common_labels
}

resource "google_compute_instance" "openeocraft" {
  name         = "${local.name_prefix}-vm"
  machine_type = var.machine_type
  zone         = var.zone
  tags         = [local.name_prefix]
  labels       = local.common_labels

  boot_disk {
    initialize_params {
      image = "ubuntu-os-cloud/ubuntu-2204-lts"
      size  = var.boot_disk_size_gb
      type  = "pd-balanced"
    }
  }

  dynamic "attached_disk" {
    for_each = var.workspace_disk_size_gb > 0 ? [1] : []
    content {
      source      = google_compute_disk.workspace[0].self_link
      device_name = "workspace"
    }
  }

  dynamic "guest_accelerator" {
    for_each = local.attach_external_gpu ? [1] : []
    content {
      type  = var.gpu_type
      count = var.gpu_count
    }
  }

  network_interface {
    network = var.network_name
    access_config {}
  }

  metadata = {
    startup-script = local.startup_script
    ssh-keys       = "${var.ssh_user}:${var.ssh_public_key}"
  }

  dynamic "scheduling" {
    for_each = local.attach_external_gpu ? [1] : []
    content {
      on_host_maintenance = "TERMINATE"
      automatic_restart   = true
    }
  }

  service_account {
    email  = data.google_compute_default_service_account.default.email
    scopes = ["cloud-platform"]
  }

  lifecycle {
    ignore_changes = [boot_disk[0].initialize_params[0].image]
  }
}
