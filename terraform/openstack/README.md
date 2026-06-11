# OpenEOcraft on OpenStack (Terraform)

This stack provisions a single **x86_64** Ubuntu 22.04 Nova instance on a dedicated tenant network with a floating IP, installs Docker via cloud-init, optionally installs the **NVIDIA driver + NVIDIA Container Toolkit** (GPU mode), pulls the OpenEOcraft image, and starts the API on port **8000**.

**Default target:** CPU flavor with **≥ 32 GB RAM** (`flavor_name`, default `m1.8xlarge` — verify on your cloud with `openstack flavor list`).

## Prerequisites

| Requirement | Notes |
| ----------- | ----- |
| [Terraform](https://developer.hashicorp.com/terraform/install) >= 1.5 | |
| OpenStack project with compute + networking + block storage | |
| Nova **key pair** | `openstack keypair create …` |
| Flavor with **≥ 32 GB RAM** | `openstack flavor list` — names vary by cloud |
| External network for floating IPs | Often `public` or `ext-net` |
| Your public IP as CIDR | `curl -s ifconfig.me` → `x.x.x.x/32` |

### OpenStack authentication

Set standard OpenStack environment variables (from `openstack rc` or your cloud dashboard):

```bash
export OS_AUTH_URL=...
export OS_USERNAME=...
export OS_PASSWORD=...
export OS_PROJECT_NAME=...
export OS_USER_DOMAIN_NAME=Default
export OS_PROJECT_DOMAIN_NAME=Default
export OS_REGION_NAME=...
openstack token issue
```

Or use `clouds.yaml` / application credentials as supported by the [OpenStack Terraform provider](https://registry.terraform.io/providers/terraform-provider-openstack/openstack/latest/docs).

## Quick start

```bash
cd terraform/openstack
cp terraform.tfvars.example terraform.tfvars
```

Edit `terraform.tfvars`:

- `key_pair_name` — your Nova key pair
- `flavor_name` — a flavor with **≥ 32768 MB RAM** on your cloud
- `external_network_name` / `floating_ip_pool` — your provider external network
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

**Bootstrap time:** allow **15–20 minutes** on first boot when `enable_gpu = true`.

## What gets created

| Resource | Purpose |
| -------- | ------- |
| Network, subnet, router | Tenant network with external gateway |
| Security group | SSH (22) and OpenEOcraft API (8000) from your CIDR lists |
| Nova instance | Ubuntu 22.04, Docker, optional NVIDIA stack, OpenEOcraft container |
| Cinder volume (optional) | Persistent `/var/openeo/workspace` on `/dev/vdb` (default 200 GB) |
| Floating IP | Public access to the API |

## Variables

See [`variables.tf`](variables.tf). Key settings:

| Variable | Default | Notes |
| -------- | ------- | ----- |
| `flavor_name` | `m1.8xlarge` | Must exist on your cloud with **≥ 32 GB RAM** |
| `enable_gpu` | `false` | Set `true` only with a GPU flavor; names are cloud-specific |
| `docker_cpus` / `docker_memory_gb` | `8` / `30` | Container limits |
| `workspace_volume_size_gb` | `200` | Set to `0` to skip separate Cinder volume |
| `external_network_name` | `public` | Provider external network for router + floating IP |

### GPU (optional)

GPU flavor names differ widely between OpenStack deployments. Copy [`terraform.gpu.tfvars.example`](terraform.gpu.tfvars.example), set `flavor_name` to a GPU flavor from your cloud, and set `enable_gpu = true`.

## Verify after apply

```bash
ssh ubuntu@$(terraform output -raw public_ip)

nvidia-smi   # when enable_gpu = true
sudo docker ps
sudo docker logs openeocraft --tail 50
curl -s -o /dev/null -w "%{http_code}\n" http://localhost:8000/
```

## Teardown

```bash
terraform destroy
```
