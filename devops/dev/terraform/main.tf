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
resource "aws_vpc" "iele_testnet-dev-VPC" {
  cidr_block           = "${var.vpc_cidr}"
  enable_dns_hostnames = true

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-VPC"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
  }
}

# Internet Gateway
resource "aws_internet_gateway" "iele_testnet-dev-IGW" {
  vpc_id = "${aws_vpc.iele_testnet-dev-VPC.id}"

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-IGW"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
  }
}

# Route Table
resource "aws_route_table" "iele_testnet-dev-RT" {
  vpc_id = "${aws_vpc.iele_testnet-dev-VPC.id}"

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-RT"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
  }
}

## Public Route
resource "aws_route" "iele_testnet-dev-RT-public" {
  route_table_id = "${aws_route_table.iele_testnet-dev-RT.id}"

  destination_cidr_block = "0.0.0.0/0"
  gateway_id             = "${aws_internet_gateway.iele_testnet-dev-IGW.id}"
}

# Public Subnet
resource "aws_subnet" "iele_testnet-dev-SN-public" {
  vpc_id = "${aws_vpc.iele_testnet-dev-VPC.id}"

  cidr_block = "10.0.1.0/24"

  map_public_ip_on_launch = true

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-SN-public"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
  }
}

# Security Group
resource "aws_security_group" "iele_testnet-dev-SecG" {
  vpc_id = "${aws_vpc.iele_testnet-dev-VPC.id}"

  ## inbound: http
  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound: https
  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound: ssh
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## outgoing: all
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-SecG"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
  }
}

# VM
resource "aws_instance" "iele_testnet-dev-VM" {
  count = 5

  ami = "${lookup(var.aws_amis, var.aws_region)}"

  instance_type = "${var.aws_instance_type}"
  subnet_id     = "${aws_subnet.iele_testnet-dev-SN-public.id}"

  associate_public_ip_address = true

  vpc_security_group_ids = [
    "${aws_security_group.iele_testnet-dev-SecG.id}",
  ]

  key_name = "${var.key_name}"

  tags = {
    Name        = "${var.iele_project}-${var.iele_env}-VM"
    Project     = "${var.iele_project}"
    Environment = "${var.iele_env}"
    Software    = "Mantis"
  }
}
