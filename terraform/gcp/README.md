# OpenEOcraft on Google Compute Engine (Terraform)

This stack provisions a single **x86_64** Ubuntu 22.04 GCE instance on the default VPC, installs Docker via startup script, installs the **NVIDIA driver + NVIDIA Container Toolkit** (GPU mode), pulls the OpenEOcraft image, and starts the API on port **8000**.

**Default target:** `g2-standard-8` (8 vCPU, **32 GB RAM**, 1× NVIDIA L4) with `docker run --gpus all`.

## Prerequisites

| Requirement | Notes |
| ----------- | ----- |
| [Terraform](https://developer.hashicorp.com/terraform/install) >= 1.5 | |
| GCP project with billing enabled | GPU quota may need to be requested |
| SSH public key | Paste into `ssh_public_key` in `terraform.tfvars` |
| Your public IP as CIDR | `curl -s ifconfig.me` → `x.x.x.x/32` for SSH and API rules |

### GCP authentication

```bash
gcloud auth application-default login
gcloud config set project my-gcp-project
```

Or set `GOOGLE_APPLICATION_CREDENTIALS` to a service-account JSON key for CI.

Enable required APIs before first apply:

```bash
gcloud services enable compute.googleapis.com
```

## Quick start (GPU)

```bash
cd terraform/gcp
cp terraform.tfvars.example terraform.tfvars
```

Edit `terraform.tfvars`:

- `project_id` — your GCP project ID
- `ssh_public_key` — contents of your `~/.ssh/id_ed25519.pub` (or `id_rsa.pub`)
- `ssh_cidr_blocks` / `api_cidr_blocks` — your public IP `/32`

```bash
terraform init
terraform plan
terraform apply
```

After apply:

```bash
terraform output api_url
```

**Bootstrap time:** allow **15–20 minutes** on first boot (NVIDIA drivers, Docker pull, R/torch startup).

## What gets created

| Resource | Purpose |
| -------- | ------- |
| Firewall rules | SSH (22) and OpenEOcraft API (8000) from your CIDR lists |
| Compute instance | Ubuntu 22.04, Docker, NVIDIA stack, OpenEOcraft container |
| Persistent disk (optional) | `/var/openeo/workspace` (default 200 GB) |

## Variables

See [`variables.tf`](variables.tf). Defaults target GPU with **32 GB instance RAM**:

| Variable | Default | Notes |
| -------- | ------- | ----- |
| `enable_gpu` | `true` | Installs NVIDIA stack; container runs with `--gpus all` |
| `machine_type` | `g2-standard-8` | 8 vCPU, **32 GB RAM**, 1× L4 (built-in GPU) |
| `docker_cpus` / `docker_memory_gb` | `8` / `30` | Container limits; 30 GB leaves headroom for the OS |
| `boot_disk_size_gb` | `120` | Boot disk for OS, drivers, Docker image |
| `workspace_disk_size_gb` | `200` | Set to `0` to skip separate data disk |

For non-G2 GPU types (e.g. `n1-highmem-8` + T4), set `machine_type`, `gpu_type`, and `gpu_count`; external GPUs require `on_host_maintenance = TERMINATE`.

### CPU fallback (no GPU quota yet)

Use at least **32 GB RAM**. Copy [`terraform.cpu.tfvars.example`](terraform.cpu.tfvars.example):

```hcl
enable_gpu   = false
machine_type = "n2-standard-8"   # 32 GB RAM
docker_memory_gb = 30
```

## Verify after apply

```bash
ssh ubuntu@$(terraform output -raw public_ip)

nvidia-smi
sudo docker ps
sudo docker logs openeocraft --tail 50
curl -s -o /dev/null -w "%{http_code}\n" http://localhost:8000/
```

## Teardown

```bash
terraform destroy
```
