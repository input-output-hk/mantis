variable "iele_project" {
  default = "iele_testnet"
}

variable "iele_env" {
  default = "dev"
}

variable "aws_region" {
  default = "eu-west-1"
}

variable "aws_amis" {
  # We use Packer to generate the appropriate AMI.

  default = {
    "eu-west-1" = "ami-0758c79ba2fe47137"
  }
}

variable "aws_instance_type" {
  default = "t2.large"
}

variable "key_name" {
  default     = "iohk-iele_testnet"
  description = "the ssh key to use in iele_testnet-dev EC2 machines"
}

variable "vpc_cidr" {
  default = "10.0.0.0/16"
}
