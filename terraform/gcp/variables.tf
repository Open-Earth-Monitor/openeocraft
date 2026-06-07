variable "project_id" {
  description = "GCP project ID."
  type        = string
}

variable "region" {
  description = "GCP region for the OpenEOcraft instance."
  type        = string
  default     = "us-central1"
}

variable "zone" {
  description = "GCP zone for the OpenEOcraft instance."
  type        = string
  default     = "us-central1-a"
}

variable "project_name" {
  description = "Prefix used for resource names and labels."
  type        = string
  default     = "openeocraft-gpu"
}

variable "machine_type" {
  description = "GCE machine type. Must be x86_64 for torch/linux/amd64. Default g2-standard-8 provides 32 GB RAM and one NVIDIA L4 GPU."
  type        = string
  default     = "g2-standard-8"
}

variable "gpu_type" {
  description = "GPU accelerator type when enable_gpu is true and machine_type does not include a built-in GPU (G2). Ignored for g2-* machine types."
  type        = string
  default     = "nvidia-tesla-t4"
}

variable "gpu_count" {
  description = "Number of GPUs to attach when using a non-G2 machine type with enable_gpu."
  type        = number
  default     = 1
}

variable "ssh_user" {
  description = "SSH username for the instance."
  type        = string
  default     = "ubuntu"
}

variable "ssh_public_key" {
  description = "SSH public key content for instance authentication."
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
  description = "Memory limit passed to docker run (-m), in gigabytes. Set to 0 to omit the limit. Default 30 GB on a 32 GB VM leaves headroom for the OS."
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

variable "boot_disk_size_gb" {
  description = "Boot disk size in GB."
  type        = number
  default     = 120
}

variable "workspace_disk_size_gb" {
  description = "Optional persistent disk for /var/openeo/workspace. Set to 0 to disable."
  type        = number
  default     = 200
}

variable "network_name" {
  description = "VPC network name. Uses the default VPC when set to 'default'."
  type        = string
  default     = "default"
}

variable "labels" {
  description = "Additional labels applied to all resources."
  type        = map(string)
  default     = {}
}
