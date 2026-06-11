variable "region" {
  description = "OpenStack region name."
  type        = string
  default     = ""
}

variable "project_name" {
  description = "Prefix used for resource names and tags."
  type        = string
  default     = "openeocraft"
}

variable "flavor_name" {
  description = "Nova flavor with at least 32 GB RAM (x86_64). Run `openstack flavor list` on your cloud and pick a matching name."
  type        = string
  default     = "m1.8xlarge"
}

variable "image_name" {
  description = "Glance image name for Ubuntu 22.04 x86_64."
  type        = string
  default     = "Ubuntu 22.04"
}

variable "key_pair_name" {
  description = "Name of an existing Nova key pair for SSH access."
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
  description = "Install NVIDIA drivers + container runtime and run OpenEOcraft with --gpus all. Requires a GPU flavor on your OpenStack cloud."
  type        = bool
  default     = false
}

variable "nvidia_driver_major" {
  description = "NVIDIA driver major version to install on Ubuntu (for example 535)."
  type        = number
  default     = 535
}

variable "root_volume_size_gb" {
  description = "Root disk size in GB."
  type        = number
  default     = 120
}

variable "workspace_volume_size_gb" {
  description = "Optional Cinder volume for /var/openeo/workspace. Set to 0 to disable."
  type        = number
  default     = 200
}

variable "external_network_name" {
  description = "Name of the provider external network used for floating IPs and router gateway."
  type        = string
  default     = "public"
}

variable "floating_ip_pool" {
  description = "Floating IP pool (usually the same as external_network_name)."
  type        = string
  default     = "public"
}

variable "network_cidr" {
  description = "CIDR for the tenant network created by this stack."
  type        = string
  default     = "10.0.0.0/24"
}

variable "dns_nameservers" {
  description = "DNS servers for the tenant subnet."
  type        = list(string)
  default     = ["1.1.1.1", "8.8.8.8"]
}

variable "tags" {
  description = "Additional metadata tags applied to the instance."
  type        = map(string)
  default     = {}
}
