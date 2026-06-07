terraform {
  required_version = ">= 1.5.0"

  required_providers {
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 4.0"
    }
  }

  # Uncomment and configure for team/production use (Azure Storage + blob locking).
  # backend "azurerm" {
  #   resource_group_name  = "tfstate"
  #   storage_account_name = "mytfstate"
  #   container_name       = "tfstate"
  #   key                  = "openeocraft/azure/terraform.tfstate"
  # }
}

provider "azurerm" {
  features {}
}
