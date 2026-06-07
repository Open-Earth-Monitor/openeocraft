output "resource_group_name" {
  description = "Azure resource group name."
  value       = azurerm_resource_group.openeocraft.name
}

output "vm_id" {
  description = "Azure VM resource ID."
  value       = azurerm_linux_virtual_machine.openeocraft.id
}

output "public_ip" {
  description = "Public IPv4 address of the OpenEOcraft server."
  value       = azurerm_public_ip.openeocraft.ip_address
}

output "api_url" {
  description = "OpenEOcraft API base URL."
  value       = "http://${azurerm_public_ip.openeocraft.ip_address}:${var.api_port}"
}

output "ssh_command" {
  description = "Example SSH command using the configured admin user."
  value       = "ssh ${var.admin_username}@${azurerm_public_ip.openeocraft.ip_address}"
}

output "network_security_group_id" {
  description = "Network security group attached to the VM."
  value       = azurerm_network_security_group.openeocraft.id
}

output "workspace_disk_id" {
  description = "Managed disk ID for persistent workspace data, if enabled."
  value       = try(azurerm_managed_disk.workspace[0].id, null)
}

output "gpu_enabled" {
  description = "Whether GPU bootstrap is enabled."
  value       = var.enable_gpu
}
