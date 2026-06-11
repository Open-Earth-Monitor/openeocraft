terraform {
  required_version = ">= 1.5.0"

  required_providers {
    openstack = {
      source  = "terraform-provider-openstack/openstack"
      version = "~> 2.1"
    }
  }

  # Uncomment and configure for team/production use (Swift/S3-compatible object store).
  # backend "s3" {
  #   bucket = "my-tf-state"
  #   key    = "openeocraft/openstack/terraform.tfstate"
  #   region = "us-east-1"
  #   endpoints = { s3 = "https://object-store.example.com" }
  #   skip_credentials_validation = true
  #   skip_metadata_api_check     = true
  #   skip_region_validation      = true
  # }
}

provider "openstack" {}
