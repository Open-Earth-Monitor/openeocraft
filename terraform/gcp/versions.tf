terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 6.0"
    }
  }

  # Uncomment and configure for team/production use (GCS backend + state locking).
  # backend "gcs" {
  #   bucket = "my-tf-state"
  #   prefix = "openeocraft/gcp"
  # }
}

provider "google" {
  project = var.project_id
  region  = var.region
  zone    = var.zone
}
