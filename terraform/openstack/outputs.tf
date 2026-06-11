output "instance_id" {
  description = "Nova instance ID."
  value       = openstack_compute_instance_v2.openeocraft.id
}

output "instance_name" {
  description = "Nova instance name."
  value       = openstack_compute_instance_v2.openeocraft.name
}

output "public_ip" {
  description = "Floating IPv4 address of the OpenEOcraft server."
  value       = openstack_networking_floatingip_v2.openeocraft.address
}

output "private_ip" {
  description = "Tenant network IPv4 address of the instance."
  value       = openstack_compute_instance_v2.openeocraft.network[0].fixed_ip_v4
}

output "api_url" {
  description = "OpenEOcraft API base URL."
  value       = "http://${openstack_networking_floatingip_v2.openeocraft.address}:${var.api_port}"
}

output "ssh_command" {
  description = "Example SSH command using the configured key pair."
  value       = "ssh ubuntu@${openstack_networking_floatingip_v2.openeocraft.address}"
}

output "security_group_id" {
  description = "Security group attached to the instance."
  value       = openstack_networking_secgroup_v2.openeocraft.id
}

output "workspace_volume_id" {
  description = "Cinder volume ID for persistent workspace data, if enabled."
  value       = try(openstack_blockstorage_volume_v3.workspace[0].id, null)
}

output "flavor_name" {
  description = "Nova flavor used for the instance."
  value       = data.openstack_compute_flavor_v2.openeocraft.name
}

output "flavor_ram_mb" {
  description = "RAM (MB) of the selected flavor."
  value       = data.openstack_compute_flavor_v2.openeocraft.ram
}

output "gpu_enabled" {
  description = "Whether GPU bootstrap is enabled."
  value       = var.enable_gpu
}
