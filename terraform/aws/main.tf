data "aws_caller_identity" "current" {}

data "aws_vpc" "default" {
  default = true
}

data "aws_subnets" "default" {
  filter {
    name   = "vpc-id"
    values = [data.aws_vpc.default.id]
  }
}

data "aws_ami" "ubuntu" {
  most_recent = true
  owners      = ["099720109477"] # Canonical

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-jammy-22.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }
}

locals {
  name_prefix = var.project_name
  common_tags = merge(
    {
      Project     = var.project_name
      ManagedBy   = "terraform"
      Application = "openeocraft"
    },
    var.tags
  )
  subnet_id = sort(data.aws_subnets.default.ids)[0]
}

resource "aws_security_group" "openeocraft" {
  name        = "${local.name_prefix}-sg"
  description = "SSH and OpenEOcraft API access"
  vpc_id      = data.aws_vpc.default.id

  ingress {
    description = "SSH"
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = var.ssh_cidr_blocks
  }

  ingress {
    description = "OpenEOcraft API"
    from_port   = var.api_port
    to_port     = var.api_port
    protocol    = "tcp"
    cidr_blocks = var.api_cidr_blocks
  }

  egress {
    description = "All outbound traffic"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-sg"
  })
}

resource "aws_iam_role" "ssm" {
  count = var.enable_ssm ? 1 : 0

  name = "${local.name_prefix}-ssm-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Principal = {
          Service = "ec2.amazonaws.com"
        }
        Action = "sts:AssumeRole"
      }
    ]
  })

  tags = local.common_tags
}

resource "aws_iam_role_policy_attachment" "ssm" {
  count = var.enable_ssm ? 1 : 0

  role       = aws_iam_role.ssm[0].name
  policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
}

resource "aws_iam_instance_profile" "ssm" {
  count = var.enable_ssm ? 1 : 0

  name = "${local.name_prefix}-ssm-profile"
  role = aws_iam_role.ssm[0].name
}

resource "aws_instance" "openeocraft" {
  ami                         = data.aws_ami.ubuntu.id
  instance_type               = var.instance_type
  subnet_id                   = local.subnet_id
  vpc_security_group_ids      = [aws_security_group.openeocraft.id]
  key_name                    = var.key_name
  associate_public_ip_address = var.associate_public_ip
  iam_instance_profile        = var.enable_ssm ? aws_iam_instance_profile.ssm[0].name : null

  user_data = templatefile("${path.module}/user_data.tftpl", {
    docker_image        = var.docker_image
    docker_cpus         = var.docker_cpus
    docker_memory_gb    = var.docker_memory_gb
    api_port            = var.api_port
    mount_workspace     = var.workspace_volume_size_gb > 0
    workspace_device    = "/dev/sdf"
    enable_gpu          = var.enable_gpu
    nvidia_driver_major = var.nvidia_driver_major
    swap_size_gb        = var.swap_size_gb
  })

  user_data_replace_on_change = true

  root_block_device {
    volume_size           = var.root_volume_size_gb
    volume_type           = "gp3"
    encrypted             = true
    delete_on_termination = true
  }

  metadata_options {
    http_endpoint = "enabled"
    http_tokens   = "required"
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-ec2"
  })

  lifecycle {
    ignore_changes = [ami]
  }
}

resource "aws_ebs_volume" "workspace" {
  count = var.workspace_volume_size_gb > 0 ? 1 : 0

  availability_zone = aws_instance.openeocraft.availability_zone
  size              = var.workspace_volume_size_gb
  type              = "gp3"
  encrypted         = true

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-workspace"
  })
}

resource "aws_volume_attachment" "workspace" {
  count = var.workspace_volume_size_gb > 0 ? 1 : 0

  device_name = "/dev/sdf"
  volume_id   = aws_ebs_volume.workspace[0].id
  instance_id = aws_instance.openeocraft.id
}
