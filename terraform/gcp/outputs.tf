output "instance_name" {
  description = "GCE instance name."
  value       = google_compute_instance.openeocraft.name
}

output "instance_id" {
  description = "GCE instance ID."
  value       = google_compute_instance.openeocraft.instance_id
}

output "public_ip" {
  description = "Public IPv4 address of the OpenEOcraft server."
  value       = google_compute_instance.openeocraft.network_interface[0].access_config[0].nat_ip
}

output "api_url" {
  description = "OpenEOcraft API base URL."
  value       = "http://${google_compute_instance.openeocraft.network_interface[0].access_config[0].nat_ip}:${var.api_port}"
}

output "ssh_command" {
  description = "Example SSH command using the configured user."
  value       = "ssh ${var.ssh_user}@${google_compute_instance.openeocraft.network_interface[0].access_config[0].nat_ip}"
}

output "workspace_disk_name" {
  description = "Persistent disk name for workspace data, if enabled."
  value       = try(google_compute_disk.workspace[0].name, null)
}

output "gpu_enabled" {
  description = "Whether GPU bootstrap is enabled."
  value       = var.enable_gpu
}
