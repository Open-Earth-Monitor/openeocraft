# OpenEOcraft on Azure VM (Terraform)

This stack provisions a single **x86_64** Ubuntu 22.04 VM with a public IP, installs Docker via cloud-init, installs the **NVIDIA driver + NVIDIA Container Toolkit** (GPU mode), pulls the OpenEOcraft image, and starts the API on port **8000**.

**Default target:** `Standard_NC8as_T4_v3` (8 vCPU, **56 GB RAM**, 1× NVIDIA T4) with `docker run --gpus all`.

## Prerequisites

| Requirement | Notes |
| ----------- | ----- |
| [Terraform](https://developer.hashicorp.com/terraform/install) >= 1.5 | |
| Azure subscription with billing | GPU NC-series may require quota increase |
| SSH public key | Paste into `ssh_public_key` in `terraform.tfvars` |
| Your public IP as CIDR | `curl -s ifconfig.me` → `x.x.x.x/32` for SSH and API rules |

### Azure authentication

```bash
az login
az account set --subscription "<subscription-id>"
az account show
```

Or set service-principal environment variables for CI (`ARM_CLIENT_ID`, `ARM_CLIENT_SECRET`, `ARM_SUBSCRIPTION_ID`, `ARM_TENANT_ID`).

## Quick start (GPU)

```bash
cd terraform/azure
cp terraform.tfvars.example terraform.tfvars
```

Edit `terraform.tfvars`:

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
| Resource group + VNet + subnet | Isolated network with static public IP |
| Network security group | SSH (22) and OpenEOcraft API (8000) from your CIDR lists |
| Linux VM | Ubuntu 22.04, Docker, NVIDIA stack, OpenEOcraft container |
| Managed disk (optional) | Persistent `/var/openeo/workspace` (default 200 GB) |

## Variables

See [`variables.tf`](variables.tf). Defaults target GPU with **32+ GB instance RAM**:

| Variable | Default | Notes |
| -------- | ------- | ----- |
| `enable_gpu` | `true` | Installs NVIDIA stack; container runs with `--gpus all` |
| `vm_size` | `Standard_NC8as_T4_v3` | 8 vCPU, **56 GB RAM**, 1× T4; x64 only |
| `docker_cpus` / `docker_memory_gb` | `8` / `30` | Container limits; 30 GB leaves headroom for the OS |
| `os_disk_size_gb` | `120` | OS disk for drivers and Docker image |
| `workspace_disk_size_gb` | `200` | Set to `0` to skip separate data disk |

### CPU fallback (no GPU quota yet)

Use at least **32 GB RAM**. Copy [`terraform.cpu.tfvars.example`](terraform.cpu.tfvars.example):

```hcl
enable_gpu = false
vm_size    = "Standard_D8s_v5"   # 32 GB RAM
docker_memory_gb = 30
```

## Verify after apply

```bash
ssh azureuser@$(terraform output -raw public_ip)

nvidia-smi
sudo docker ps
sudo docker logs openeocraft --tail 50
curl -s -o /dev/null -w "%{http_code}\n" http://localhost:8000/
```

## Teardown

```bash
terraform destroy
```
