variable "location" {
  description = "Azure region for the OpenEOcraft VM."
  type        = string
  default     = "eastus"
}

variable "project_name" {
  description = "Prefix used for resource names and tags."
  type        = string
  default     = "openeocraft-gpu"
}

variable "vm_size" {
  description = "Azure VM size. Must be x64 (not Arm) for torch/linux/amd64. Default Standard_NC8as_T4_v3 provides 56 GB RAM and one NVIDIA T4 GPU."
  type        = string
  default     = "Standard_NC8as_T4_v3"
}

variable "admin_username" {
  description = "Admin username for SSH access."
  type        = string
  default     = "azureuser"
}

variable "ssh_public_key" {
  description = "SSH public key content for VM authentication."
  type        = string
}

variable "ssh_cidr_blocks" {
  description = "CIDR blocks allowed to connect on SSH (port 22)."
  type        = list(string)

  validation {
    condition     = length(var.ssh_cidr_blocks) > 0
    error_message = "Provide at least one CIDR block for SSH access."
  }
}

variable "api_cidr_blocks" {
  description = "CIDR blocks allowed to reach the OpenEOcraft API (port 8000)."
  type        = list(string)

  validation {
    condition     = length(var.api_cidr_blocks) > 0
    error_message = "Provide at least one CIDR block for API access."
  }
}

variable "api_port" {
  description = "Port exposed by the OpenEOcraft container."
  type        = number
  default     = 8000
}

variable "docker_image" {
  description = "Docker image for OpenEOcraft."
  type        = string
  default     = "brianpondi/openeocraft:latest"
}

variable "docker_cpus" {
  description = "CPU limit passed to docker run (--cpus)."
  type        = number
  default     = 8
}

variable "docker_memory_gb" {
  description = "Memory limit passed to docker run (-m), in gigabytes. Set to 0 to omit the limit. Default 30 GB on a 32+ GB VM leaves headroom for the OS."
  type        = number
  default     = 30

  validation {
    condition     = var.docker_memory_gb == 0 || var.docker_memory_gb >= 4
    error_message = "docker_memory_gb must be 0 (no limit) or at least 4."
  }
}

variable "swap_size_gb" {
  description = "Swap file size in GB added during bootstrap. Use on very small CPU instances only; keep 0 for GPU."
  type        = number
  default     = 0
}

variable "enable_gpu" {
  description = "Install NVIDIA drivers + container runtime and run OpenEOcraft with --gpus all."
  type        = bool
  default     = true
}

variable "nvidia_driver_major" {
  description = "NVIDIA driver major version to install on Ubuntu (for example 535)."
  type        = number
  default     = 535
}

variable "os_disk_size_gb" {
  description = "OS managed disk size in GB."
  type        = number
  default     = 120
}

variable "workspace_disk_size_gb" {
  description = "Optional separate managed disk for /var/openeo/workspace. Set to 0 to disable."
  type        = number
  default     = 200
}

variable "tags" {
  description = "Additional tags applied to all resources."
  type        = map(string)
  default     = {}
}
