###############################################################################
#
# Useful resources:
#
# - https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Scenarios.html
# - https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html
# - https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html
#
# - https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/vpc-nat-gateway.html
#
# - https://github.com/terraform-providers/terraform-provider-aws/blob/master/examples/two-tier/main.tf
# - https://github.com/terraform-aws-modules/terraform-aws-vpc/tree/master/examples/simple-vpc
# - https://github.com/terraform-aws-modules/terraform-aws-vpc/tree/master/examples/complete-vpc
# - https://github.com/terraform-aws-modules/terraform-aws-vpc/blob/master/main.tf
# - https://github.com/terraform-aws-modules/terraform-aws-elb
#
###############################################################################

terraform {
  required_version = ">= 0.10.3"
}

provider "aws" {
  region = "${var.aws_region}"
}

# VPC
resource "aws_vpc" "iele_testnet-dev-v2-VPC" {
  cidr_block           = "${var.vpc_cidr}"
  enable_dns_hostnames = true

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-${var.version}-VPC"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
    Version     = "${var.version}"
  }
}

# Internet Gateway
resource "aws_internet_gateway" "iele_testnet-dev-v2-IGW" {
  vpc_id = "${aws_vpc.iele_testnet-dev-v2-VPC.id}"

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-${var.version}-IGW"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
    Version     = "${var.version}"
  }
}

# Route Table
resource "aws_route_table" "iele_testnet-dev-v2-RT" {
  vpc_id = "${aws_vpc.iele_testnet-dev-v2-VPC.id}"

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-${var.version}-RT"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
    Version     = "${var.version}"
  }
}

## Public Route
resource "aws_route" "iele_testnet-dev-RT-v2-public" {
  route_table_id = "${aws_vpc.iele_testnet-dev-v2-VPC.main_route_table_id}"

  destination_cidr_block = "0.0.0.0/0"
  gateway_id             = "${aws_internet_gateway.iele_testnet-dev-v2-IGW.id}"
}

# Public Subnet
resource "aws_subnet" "iele_testnet-dev-v2-SN-public" {
  vpc_id = "${aws_vpc.iele_testnet-dev-v2-VPC.id}"

  cidr_block = "10.0.1.0/24"

  map_public_ip_on_launch = true

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-${var.version}-SN-public"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
    Version     = "${var.version}"
  }
}

# Security Group
resource "aws_security_group" "iele_testnet-dev-v2-SecG" {
  vpc_id = "${aws_vpc.iele_testnet-dev-v2-VPC.id}"

  ## inbound (world): http
  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound (world): https
  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound (world): ssh
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound (VPC): all
  ingress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["10.0.0.0/16"]
  }

  ## outgoing: all
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-${var.version}-SecG"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
    Version     = "${var.version}"
  }
}

data "template_file" "authorized_keys" {
  template = "${file("${path.module}/authorized_keys")}"
}

# VM
resource "aws_instance" "iele_testnet-dev-v2-VM" {
  count = 5

  ami = "${lookup(var.aws_amis, var.aws_region)}"

  instance_type = "${var.aws_instance_type}"
  subnet_id     = "${aws_subnet.iele_testnet-dev-v2-SN-public.id}"

  associate_public_ip_address = true

  vpc_security_group_ids = [
    "${aws_security_group.iele_testnet-dev-v2-SecG.id}",
  ]

  key_name = "${var.key_name}"

  provisioner "file" {
    destination = ".ssh/authorized_keys"
    content     = "${data.template_file.authorized_keys.rendered}"
  }

  connection {
    user = "ubuntu"
  }

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-${var.version}-${element(var.vm_names, count.index)}"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
    Version     = "${var.version}"
  }
}
