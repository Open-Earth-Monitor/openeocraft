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
  cloud_init = templatefile("${path.module}/../shared/cloud_init.tftpl", {
    docker_image        = var.docker_image
    docker_cpus         = var.docker_cpus
    docker_memory_gb    = var.docker_memory_gb
    api_port            = var.api_port
    mount_workspace     = var.workspace_disk_size_gb > 0
    workspace_device    = "/dev/disk/azure/scsi1/lun10"
    enable_gpu          = var.enable_gpu
    nvidia_driver_major = var.nvidia_driver_major
    swap_size_gb        = var.swap_size_gb
  })
}

resource "azurerm_resource_group" "openeocraft" {
  name     = "${local.name_prefix}-rg"
  location = var.location
  tags     = local.common_tags
}

resource "azurerm_virtual_network" "openeocraft" {
  name                = "${local.name_prefix}-vnet"
  address_space       = ["10.0.0.0/16"]
  location            = azurerm_resource_group.openeocraft.location
  resource_group_name = azurerm_resource_group.openeocraft.name
  tags                = local.common_tags
}

resource "azurerm_subnet" "openeocraft" {
  name                 = "${local.name_prefix}-subnet"
  resource_group_name  = azurerm_resource_group.openeocraft.name
  virtual_network_name = azurerm_virtual_network.openeocraft.name
  address_prefixes     = ["10.0.1.0/24"]
}

resource "azurerm_public_ip" "openeocraft" {
  name                = "${local.name_prefix}-pip"
  location            = azurerm_resource_group.openeocraft.location
  resource_group_name = azurerm_resource_group.openeocraft.name
  allocation_method   = "Static"
  sku                 = "Standard"
  tags                = local.common_tags
}

resource "azurerm_network_security_group" "openeocraft" {
  name                = "${local.name_prefix}-nsg"
  location            = azurerm_resource_group.openeocraft.location
  resource_group_name = azurerm_resource_group.openeocraft.name
  tags                = local.common_tags

  dynamic "security_rule" {
    for_each = { for idx, cidr in var.ssh_cidr_blocks : cidr => idx }
    content {
      name                       = "ssh-${replace(security_rule.key, "/", "-")}"
      priority                   = 1000 + security_rule.value
      direction                  = "Inbound"
      access                     = "Allow"
      protocol                   = "Tcp"
      source_port_range          = "*"
      destination_port_range     = "22"
      source_address_prefix      = security_rule.key
      destination_address_prefix = "*"
    }
  }

  dynamic "security_rule" {
    for_each = { for idx, cidr in var.api_cidr_blocks : cidr => idx }
    content {
      name                       = "api-${replace(security_rule.key, "/", "-")}"
      priority                   = 2000 + security_rule.value
      direction                  = "Inbound"
      access                     = "Allow"
      protocol                   = "Tcp"
      source_port_range          = "*"
      destination_port_range     = tostring(var.api_port)
      source_address_prefix      = security_rule.key
      destination_address_prefix = "*"
    }
  }
}

resource "azurerm_network_interface" "openeocraft" {
  name                = "${local.name_prefix}-nic"
  location            = azurerm_resource_group.openeocraft.location
  resource_group_name = azurerm_resource_group.openeocraft.name
  tags                = local.common_tags

  ip_configuration {
    name                          = "internal"
    subnet_id                     = azurerm_subnet.openeocraft.id
    private_ip_address_allocation = "Dynamic"
    public_ip_address_id          = azurerm_public_ip.openeocraft.id
  }
}

resource "azurerm_network_interface_security_group_association" "openeocraft" {
  network_interface_id      = azurerm_network_interface.openeocraft.id
  network_security_group_id = azurerm_network_security_group.openeocraft.id
}

resource "azurerm_managed_disk" "workspace" {
  count = var.workspace_disk_size_gb > 0 ? 1 : 0

  name                 = "${local.name_prefix}-workspace"
  location             = azurerm_resource_group.openeocraft.location
  resource_group_name  = azurerm_resource_group.openeocraft.name
  storage_account_type = "Premium_LRS"
  create_option        = "Empty"
  disk_size_gb         = var.workspace_disk_size_gb
  tags                 = local.common_tags
}

resource "azurerm_linux_virtual_machine" "openeocraft" {
  name                = "${local.name_prefix}-vm"
  location            = azurerm_resource_group.openeocraft.location
  resource_group_name = azurerm_resource_group.openeocraft.name
  size                = var.vm_size
  admin_username      = var.admin_username
  tags                = local.common_tags

  network_interface_ids = [
    azurerm_network_interface.openeocraft.id,
  ]

  admin_ssh_key {
    username   = var.admin_username
    public_key = var.ssh_public_key
  }

  os_disk {
    caching              = "ReadWrite"
    storage_account_type = "Premium_LRS"
    disk_size_gb         = var.os_disk_size_gb
  }

  source_image_reference {
    publisher = "Canonical"
    offer     = "0001-com-ubuntu-server-jammy"
    sku       = "22_04-lts-gen2"
    version   = "latest"
  }

  custom_data = base64encode(local.cloud_init)

  lifecycle {
    ignore_changes = [source_image_reference]
  }
}

resource "azurerm_virtual_machine_data_disk_attachment" "workspace" {
  count = var.workspace_disk_size_gb > 0 ? 1 : 0

  managed_disk_id    = azurerm_managed_disk.workspace[0].id
  virtual_machine_id = azurerm_linux_virtual_machine.openeocraft.id
  lun                = 10
  caching            = "ReadWrite"
}
