# OpenEOcraft on AWS EC2 (Terraform)

This stack provisions a single **x86_64** EC2 instance in the **default VPC**, installs Docker via cloud-init, installs the **NVIDIA driver + NVIDIA Container Toolkit** (GPU mode), pulls the OpenEOcraft image, and starts the API on port **8000**.

**Default target:** `g4dn.xlarge` (4 vCPU, **16 GB RAM**, 1× NVIDIA T4) with `docker run --gpus all`.

## Prerequisites

| Requirement | Notes |
| ----------- | ----- |
| [Terraform](https://developer.hashicorp.com/terraform/install) >= 1.5 | |
| AWS account with **payment method** | Free-tier-only accounts cannot launch GPU or most paid instance types |
| **GPU vCPU quota** | New accounts start at **0** for G/VT instances — request an increase before apply (see below) |
| EC2 **key pair** in the target region | EC2 → Key Pairs → create `.pem` |
| Your public IP as CIDR | `curl -s ifconfig.me` → `x.x.x.x/32` for SSH and API rules |

### AWS authentication

**Personal / free-tier account (no IAM Identity Center):** use IAM access keys, not SSO.

```bash
aws configure --profile openeocraft
export AWS_PROFILE=openeocraft
export AWS_REGION=us-east-1
aws sts get-caller-identity
```

Attach **AdministratorAccess** (or sufficient EC2 + IAM permissions) to the IAM user used for Terraform.

**Organization with IAM Identity Center:** use `aws configure sso --profile openeocraft` with your org’s SSO start URL (`https://….awsapps.com/start`).

## Quick start (GPU)

```bash
cd terraform/aws
cp terraform.tfvars.example terraform.tfvars
```

Edit `terraform.tfvars`:

- `key_name` — your EC2 key pair name
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

Default basic auth in the shipped image is often `user` / `password`. Change credentials before exposing the host broadly.

**Bootstrap time:** allow **15–20 minutes** on first boot (NVIDIA drivers, Docker pull, R/torch startup).

## GPU vCPU quota (required for new accounts)

If apply fails with:

```text
VcpuLimitExceeded: vCPU limit of 0 ... G and VT instances
```

request a quota increase:

1. AWS Console → **Service Quotas** → region **us-east-1**
2. **Amazon EC2** → **Running On-Demand G and VT instances**
3. **Request increase at account-level** → at least **4 vCPUs** (one `g4dn.xlarge`)

Or CLI:

```bash
aws service-quotas request-service-quota-increase \
  --profile openeocraft \
  --region us-east-1 \
  --service-code ec2 \
  --quota-code L-DB2E81BA \
  --desired-value 4
```

Check status:

```bash
aws service-quotas get-service-quota \
  --profile openeocraft \
  --region us-east-1 \
  --service-code ec2 \
  --quota-code L-DB2E81BA \
  --query 'Quota.Value'
```

Wait until the value is **≥ 4** and the request status is **APPROVED**, then `terraform apply` again.

## What gets created

| Resource | Purpose |
| -------- | ------- |
| Security group | SSH (22) and OpenEOcraft API (8000) from your CIDR lists |
| EC2 instance | Ubuntu 22.04, Docker, NVIDIA stack, OpenEOcraft container |
| EBS volume (optional) | Persistent `/var/openeo/workspace` (default 200 GB) |
| IAM role (optional) | SSM Session Manager when `enable_ssm = true` |

The stack uses the **default VPC** and the first available subnet. For production, add a dedicated VPC, load balancer, and HTTPS.

## Variables

See [`variables.tf`](variables.tf). Defaults target GPU with **16 GB instance RAM**:

| Variable | Default | Notes |
| -------- | ------- | ----- |
| `enable_gpu` | `true` | Installs NVIDIA stack; container runs with `--gpus all` |
| `instance_type` | `g4dn.xlarge` | 4 vCPU, **16 GB RAM**, 1× T4; x86_64 only (no Graviton) |
| `docker_cpus` / `docker_memory_gb` | `4` / `14` | Container limits; 14 GB leaves ~2 GB for the OS on g4dn.xlarge |
| `nvidia_driver_major` | `535` | Ubuntu driver package during bootstrap |
| `root_volume_size_gb` | `120` | Root disk for OS, drivers, Docker image |
| `workspace_volume_size_gb` | `200` | Set to `0` to skip separate data volume |
| `swap_size_gb` | `0` | Only for very small CPU fallbacks (e.g. `t3.micro`) |
| `enable_ssm` | `true` | `aws ssm start-session --target <instance-id>` |

### CPU fallback (no GPU quota yet)

Use at least **16 GB RAM** for OpenEOcraft (torch + sits). Copy [`terraform.cpu.tfvars.example`](terraform.cpu.tfvars.example):

```hcl
enable_gpu    = false
instance_type = "t3.xlarge"   # 16 GB RAM
docker_memory_gb = 14
```

Do **not** use `t3.micro` / `t3.small` — the container will OOM or never become reachable.

## GPU bootstrap (cloud-init)

When `enable_gpu = true`, user data:

1. Installs Docker
2. Installs `nvidia-driver-<major>` (default `535`)
3. Installs `nvidia-container-toolkit` and runs `nvidia-ctk runtime configure --runtime=docker`
4. Pulls `brianpondi/openeocraft:latest`
5. Starts the container with `--gpus all`

### Verify after apply

```bash
ssh -i <your-key.pem> ubuntu@$(terraform output -raw public_ip)

nvidia-smi
sudo docker ps
sudo docker logs openeocraft --tail 50
curl -s -o /dev/null -w "%{http_code}\n" http://localhost:8000/
```

From your laptop (after bootstrap completes):

```bash
curl -s -o /dev/null -w "%{http_code}\n" $(terraform output -raw api_url)
```

### Troubleshooting “site can’t be reached”

| Check | Action |
| ----- | ------ |
| Bootstrap still running | `sudo tail -f /var/log/cloud-init-output.log` — wait 15–20 min |
| Wrong IP in security group | `curl -s ifconfig.me` → update CIDRs in `terraform.tfvars` → `terraform apply` |
| Container crashed | `sudo docker ps -a` and `sudo docker logs openeocraft` |
| Instance too small | Use `g4dn.xlarge` or `t3.xlarge` (16 GB RAM minimum) |
| GPU quota pending | Wait for G/VT vCPU quota approval |

## Remote state (recommended for teams)

Uncomment the `backend "s3"` block in [`versions.tf`](versions.tf) and create an encrypted S3 bucket plus DynamoDB lock table before `terraform init -reconfigure`.

## Cost

| Instance | Approx. cost (us-east-1) |
| -------- | -------------------------- |
| `g4dn.xlarge` | ~$0.50+/hr + EBS |
| `t3.xlarge` (CPU fallback) | ~$0.17/hr |

Stop charges when finished:

```bash
terraform destroy
```

## Teardown

```bash
terraform destroy
```

Persistent workspace data on the attached EBS volume is deleted with the volume unless you snapshot it first.
