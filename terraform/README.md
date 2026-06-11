# OpenEOcraft Terraform (multi-cloud)

Repeatable provisioning for OpenEOcraft on **AWS**, **Azure**, **Google Cloud**, and **OpenStack**. Each stack launches a single **x86_64** Ubuntu 22.04 VM, installs Docker via cloud-init, optionally installs the **NVIDIA driver + NVIDIA Container Toolkit**, pulls the OpenEOcraft image, and starts the API on port **8000**.

All defaults target **at least 32 GB instance RAM** (30 GB container limit, leaving headroom for the OS).

| Cloud | Directory | Default GPU instance | Default CPU fallback |
| ----- | --------- | -------------------- | ------------------- |
| AWS | [`aws/`](aws/) | `g4dn.2xlarge` (32 GB, 1× T4) | `m5.2xlarge` (32 GB) |
| Azure | [`azure/`](azure/) | `Standard_NC8as_T4_v3` (56 GB, 1× T4) | `Standard_D8s_v5` (32 GB) |
| GCP | [`gcp/`](gcp/) | `g2-standard-8` (32 GB, 1× L4) | `n2-standard-8` (32 GB) |
| OpenStack | [`openstack/`](openstack/) | cloud-specific GPU flavor | `flavor_name` ≥ 32 GB RAM (default `m1.8xlarge`) |

Bootstrap scripts live in [`shared/cloud_init.tftpl`](shared/cloud_init.tftpl) and are shared across clouds. Workspace disks are mounted only via stable cloud-specific device paths (never heuristic disk guessing); if the data disk is not ready, bootstrap falls back to a directory on the root filesystem.

## Quick start

Pick a cloud, copy the example variables, edit credentials and CIDR blocks, then apply:

```bash
cd terraform/aws    # or azure/, gcp/, or openstack/
cp terraform.tfvars.example terraform.tfvars
# Edit terraform.tfvars (key pair / SSH key, your public IP /32)
terraform init
terraform plan
terraform apply
terraform output api_url
```

Allow **15–20 minutes** on first boot when `enable_gpu = true`.

## Prerequisites (all clouds)

| Requirement | Notes |
| ----------- | ----- |
| [Terraform](https://developer.hashicorp.com/terraform/install) >= 1.5 | |
| Cloud account with billing enabled | GPU instances require quota in most accounts |
| SSH access | AWS/OpenStack: key pair name; Azure/GCP: SSH public key |
| Your public IP as CIDR | `curl -s ifconfig.me` → `x.x.x.x/32` for SSH and API rules |

## Cloud-specific docs

- [AWS EC2](aws/README.md)
- [Azure VM](azure/README.md)
- [Google Compute Engine](gcp/README.md)
- [OpenStack Nova](openstack/README.md)

## What each stack creates

| Resource | Purpose |
| -------- | ------- |
| Network rules | SSH (22) and OpenEOcraft API (8000) from your CIDR lists |
| Compute VM | Ubuntu 22.04, Docker, optional NVIDIA stack, OpenEOcraft container |
| Data disk (optional) | Persistent `/var/openeo/workspace` (default 200 GB) |

For production, add a dedicated VPC/VNet, load balancer, and HTTPS termination in front of the API.

## Teardown

```bash
terraform destroy
```

Persistent workspace disks are deleted with the stack unless you snapshot them first.
