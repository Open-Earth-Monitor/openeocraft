data "openstack_networking_network_v2" "external" {
  name = var.external_network_name
}

data "openstack_compute_flavor_v2" "openeocraft" {
  name = var.flavor_name

  lifecycle {
    postcondition {
      condition     = self.ram >= 32768
      error_message = "flavor_name must provide at least 32 GB RAM (32768 MB); selected flavor has ${self.ram} MB."
    }
  }
}

data "openstack_images_image_v2" "ubuntu" {
  name        = var.image_name
  most_recent = true

  properties = {
    os_distro = "ubuntu"
  }
}

locals {
  name_prefix = var.project_name
  instance_tags = merge(
    {
      Project     = var.project_name
      ManagedBy   = "terraform"
      Application = "openeocraft"
    },
    var.tags
  )
  workspace_device = var.workspace_volume_size_gb > 0 ? "/dev/vdb" : ""
  cloud_init = templatefile("${path.module}/../shared/cloud_init.tftpl", {
    docker_image        = var.docker_image
    docker_cpus         = var.docker_cpus
    docker_memory_gb    = var.docker_memory_gb
    api_port            = var.api_port
    mount_workspace     = var.workspace_volume_size_gb > 0
    workspace_device    = local.workspace_device
    enable_gpu          = var.enable_gpu
    nvidia_driver_major = var.nvidia_driver_major
    swap_size_gb        = var.swap_size_gb
  })
}

resource "openstack_networking_network_v2" "openeocraft" {
  name           = "${local.name_prefix}-net"
  admin_state_up = true
}

resource "openstack_networking_subnet_v2" "openeocraft" {
  name            = "${local.name_prefix}-subnet"
  network_id      = openstack_networking_network_v2.openeocraft.id
  cidr            = var.network_cidr
  ip_version      = 4
  dns_nameservers = var.dns_nameservers
}

resource "openstack_networking_router_v2" "openeocraft" {
  name                = "${local.name_prefix}-router"
  admin_state_up      = true
  external_network_id = data.openstack_networking_network_v2.external.id
}

resource "openstack_networking_router_interface_v2" "openeocraft" {
  router_id = openstack_networking_router_v2.openeocraft.id
  subnet_id = openstack_networking_subnet_v2.openeocraft.id
}

resource "openstack_networking_secgroup_v2" "openeocraft" {
  name        = "${local.name_prefix}-sg"
  description = "SSH and OpenEOcraft API access"
}

resource "openstack_networking_secgroup_rule_v2" "ssh" {
  for_each = toset(var.ssh_cidr_blocks)

  direction         = "ingress"
  ethertype         = "IPv4"
  protocol          = "tcp"
  port_range_min    = 22
  port_range_max    = 22
  remote_ip_prefix  = each.value
  security_group_id = openstack_networking_secgroup_v2.openeocraft.id
}

resource "openstack_networking_secgroup_rule_v2" "api" {
  for_each = toset(var.api_cidr_blocks)

  direction         = "ingress"
  ethertype         = "IPv4"
  protocol          = "tcp"
  port_range_min    = var.api_port
  port_range_max    = var.api_port
  remote_ip_prefix  = each.value
  security_group_id = openstack_networking_secgroup_v2.openeocraft.id
}

resource "openstack_networking_secgroup_rule_v2" "egress" {
  direction         = "egress"
  ethertype         = "IPv4"
  remote_ip_prefix  = "0.0.0.0/0"
  security_group_id = openstack_networking_secgroup_v2.openeocraft.id
}

resource "openstack_blockstorage_volume_v3" "workspace" {
  count = var.workspace_volume_size_gb > 0 ? 1 : 0

  name              = "${local.name_prefix}-workspace"
  size              = var.workspace_volume_size_gb
  enable_online_resize = true
}

resource "openstack_compute_instance_v2" "openeocraft" {
  name            = "${local.name_prefix}-vm"
  flavor_id       = data.openstack_compute_flavor_v2.openeocraft.id
  image_id        = data.openstack_images_image_v2.ubuntu.id
  key_pair        = var.key_pair_name
  security_groups = [openstack_networking_secgroup_v2.openeocraft.name]
  user_data       = local.cloud_init

  network {
    uuid = openstack_networking_network_v2.openeocraft.id
  }

  block_device {
    uuid                  = data.openstack_images_image_v2.ubuntu.id
    source_type           = "image"
    destination_type      = "volume"
    volume_size           = var.root_volume_size_gb
    boot_index            = 0
    delete_on_termination = true
  }

  metadata = local.instance_tags

  lifecycle {
    ignore_changes = [image_id]
  }
}

resource "openstack_compute_volume_attach_v2" "workspace" {
  count = var.workspace_volume_size_gb > 0 ? 1 : 0

  instance_id = openstack_compute_instance_v2.openeocraft.id
  volume_id   = openstack_blockstorage_volume_v3.workspace[0].id
  device      = local.workspace_device
}

resource "openstack_networking_floatingip_v2" "openeocraft" {
  pool = var.floating_ip_pool
}

resource "openstack_networking_floatingip_associate_v2" "openeocraft" {
  floating_ip = openstack_networking_floatingip_v2.openeocraft.address
  port_id     = openstack_compute_instance_v2.openeocraft.network[0].port
}
