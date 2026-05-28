output "instance_id" {
  description = "EC2 instance ID."
  value       = aws_instance.openeocraft.id
}

output "public_ip" {
  description = "Public IPv4 address of the OpenEOcraft server."
  value       = aws_instance.openeocraft.public_ip
}

output "public_dns" {
  description = "Public DNS name of the OpenEOcraft server."
  value       = aws_instance.openeocraft.public_dns
}

output "api_url" {
  description = "OpenEOcraft API base URL."
  value       = "http://${aws_instance.openeocraft.public_ip}:${var.api_port}"
}

output "ssh_command" {
  description = "Example SSH command using the configured key pair."
  value       = "ssh -i <your-key.pem> ubuntu@${aws_instance.openeocraft.public_ip}"
}

output "security_group_id" {
  description = "Security group attached to the instance."
  value       = aws_security_group.openeocraft.id
}

output "workspace_volume_id" {
  description = "EBS volume ID for persistent workspace data, if enabled."
  value       = try(aws_ebs_volume.workspace[0].id, null)
}

output "gpu_enabled" {
  description = "Whether GPU bootstrap is enabled."
  value       = var.enable_gpu
}
