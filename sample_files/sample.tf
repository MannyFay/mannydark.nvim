# ==============================================================================
# Comprehensive HCL/Terraform Sample - Syntax Highlighting Demonstration
# ==============================================================================

# This file demonstrates all major HashiCorp Configuration Language features
# for syntax highlighting purposes.

# ==============================================================================
# Comments
# ==============================================================================

# Single line comment
// Another single line comment (C-style)

/* Multi-line
   block comment */

# ==============================================================================
# Terraform Settings
# ==============================================================================

terraform {
  required_version = ">= 1.0.0"

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    google = {
      source  = "hashicorp/google"
      version = ">= 4.0, < 6.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "2.23.0"
    }
    random = {
      source  = "hashicorp/random"
    }
    null = {
      source = "hashicorp/null"
    }
  }

  backend "s3" {
    bucket         = "my-terraform-state"
    key            = "prod/terraform.tfstate"
    region         = "us-west-2"
    encrypt        = true
    dynamodb_table = "terraform-locks"
  }

  # Alternative backends
  # backend "gcs" {
  #   bucket = "my-terraform-state"
  #   prefix = "terraform/state"
  # }

  # backend "azurerm" {
  #   resource_group_name  = "tfstate"
  #   storage_account_name = "tfstate12345"
  #   container_name       = "tfstate"
  #   key                  = "terraform.tfstate"
  # }

  # Cloud backend (Terraform Cloud/Enterprise)
  # cloud {
  #   organization = "my-org"
  #   workspaces {
  #     name = "my-workspace"
  #   }
  # }
}

# ==============================================================================
# Providers
# ==============================================================================

provider "aws" {
  region  = var.aws_region
  profile = var.aws_profile

  # Assume role
  assume_role {
    role_arn     = "arn:aws:iam::123456789012:role/terraform"
    session_name = "terraform-session"
    external_id  = "terraform-external-id"
  }

  # Default tags for all resources
  default_tags {
    tags = {
      Environment = var.environment
      ManagedBy   = "Terraform"
      Project     = var.project_name
    }
  }

  # Skip metadata API check
  skip_metadata_api_check     = false
  skip_region_validation      = false
  skip_credentials_validation = false
}

provider "aws" {
  alias  = "us_east_1"
  region = "us-east-1"
}

provider "google" {
  project = var.gcp_project
  region  = var.gcp_region
  zone    = var.gcp_zone
}

provider "kubernetes" {
  host                   = data.aws_eks_cluster.cluster.endpoint
  cluster_ca_certificate = base64decode(data.aws_eks_cluster.cluster.certificate_authority[0].data)
  token                  = data.aws_eks_cluster_auth.cluster.token
}

# ==============================================================================
# Variables
# ==============================================================================

# String variable
variable "aws_region" {
  description = "AWS region for resources"
  type        = string
  default     = "us-west-2"
}

# Number variable
variable "instance_count" {
  description = "Number of instances to create"
  type        = number
  default     = 3

  validation {
    condition     = var.instance_count > 0 && var.instance_count <= 10
    error_message = "Instance count must be between 1 and 10."
  }
}

# Boolean variable
variable "enable_monitoring" {
  description = "Enable CloudWatch monitoring"
  type        = bool
  default     = true
}

# List variable
variable "availability_zones" {
  description = "List of availability zones"
  type        = list(string)
  default     = ["us-west-2a", "us-west-2b", "us-west-2c"]
}

# Set variable
variable "allowed_ips" {
  description = "Set of allowed IP addresses"
  type        = set(string)
  default     = []
}

# Map variable
variable "instance_tags" {
  description = "Tags to apply to instances"
  type        = map(string)
  default = {
    Team        = "DevOps"
    CostCenter  = "12345"
  }
}

# Object variable
variable "database_config" {
  description = "Database configuration"
  type = object({
    engine         = string
    engine_version = string
    instance_class = string
    allocated_storage = number
    multi_az       = bool
    username       = string
  })
  default = {
    engine            = "postgres"
    engine_version    = "14.7"
    instance_class    = "db.t3.medium"
    allocated_storage = 100
    multi_az          = true
    username          = "admin"
  }
}

# Tuple variable
variable "cidr_blocks" {
  description = "CIDR blocks tuple"
  type        = tuple([string, string, string])
  default     = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
}

# Any type
variable "flexible_var" {
  description = "Accepts any type"
  type        = any
  default     = null
}

# Sensitive variable
variable "db_password" {
  description = "Database password"
  type        = string
  sensitive   = true
}

# Variable with complex validation
variable "environment" {
  description = "Deployment environment"
  type        = string
  default     = "dev"

  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be dev, staging, or prod."
  }
}

# Nullable variable
variable "optional_string" {
  description = "An optional string"
  type        = string
  default     = null
  nullable    = true
}

# ==============================================================================
# Locals
# ==============================================================================

locals {
  # Simple values
  project_name = "my-project"
  environment  = var.environment

  # Computed values
  name_prefix = "${local.project_name}-${local.environment}"

  # Complex expressions
  common_tags = merge(
    var.instance_tags,
    {
      Environment = local.environment
      Project     = local.project_name
      Timestamp   = timestamp()
    }
  )

  # Conditional value
  instance_type = var.environment == "prod" ? "t3.large" : "t3.micro"

  # List manipulation
  az_count = length(var.availability_zones)
  first_az = var.availability_zones[0]

  # Map manipulation
  subnet_cidrs = {
    for idx, az in var.availability_zones :
    az => cidrsubnet(var.vpc_cidr, 8, idx)
  }

  # Flattened list
  all_cidrs = flatten([
    var.cidr_blocks,
    [for subnet in aws_subnet.private : subnet.cidr_block]
  ])

  # JSON encoding
  policy_json = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect   = "Allow"
        Action   = ["s3:GetObject"]
        Resource = ["arn:aws:s3:::${local.bucket_name}/*"]
      }
    ]
  })

  # Using regex
  account_id = regex("arn:aws:.*:(\\d+):.*", data.aws_caller_identity.current.arn)[0]
}

# ==============================================================================
# Data Sources
# ==============================================================================

# AWS data sources
data "aws_caller_identity" "current" {}

data "aws_region" "current" {}

data "aws_availability_zones" "available" {
  state = "available"

  filter {
    name   = "opt-in-status"
    values = ["opt-in-not-required"]
  }
}

data "aws_ami" "amazon_linux" {
  most_recent = true
  owners      = ["amazon"]

  filter {
    name   = "name"
    values = ["amzn2-ami-hvm-*-x86_64-gp2"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }
}

data "aws_iam_policy_document" "assume_role" {
  statement {
    effect = "Allow"

    principals {
      type        = "Service"
      identifiers = ["ec2.amazonaws.com"]
    }

    actions = ["sts:AssumeRole"]

    condition {
      test     = "StringEquals"
      variable = "aws:RequestedRegion"
      values   = [var.aws_region]
    }
  }
}

data "aws_secretsmanager_secret_version" "db_credentials" {
  secret_id = aws_secretsmanager_secret.db.id
}

data "aws_eks_cluster" "cluster" {
  name = aws_eks_cluster.main.name
}

data "aws_eks_cluster_auth" "cluster" {
  name = aws_eks_cluster.main.name
}

# External data source
data "external" "example" {
  program = ["python3", "${path.module}/scripts/external.py"]

  query = {
    id = aws_instance.example.id
  }
}

# Template file
data "template_file" "user_data" {
  template = file("${path.module}/templates/user_data.sh")

  vars = {
    environment = var.environment
    region      = var.aws_region
  }
}

# ==============================================================================
# Resources
# ==============================================================================

# VPC and Networking
resource "aws_vpc" "main" {
  cidr_block           = var.vpc_cidr
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-vpc"
  })
}

resource "aws_subnet" "public" {
  count = local.az_count

  vpc_id                  = aws_vpc.main.id
  cidr_block              = cidrsubnet(var.vpc_cidr, 8, count.index)
  availability_zone       = var.availability_zones[count.index]
  map_public_ip_on_launch = true

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-public-${count.index + 1}"
    Type = "public"
  })
}

resource "aws_subnet" "private" {
  for_each = toset(var.availability_zones)

  vpc_id            = aws_vpc.main.id
  cidr_block        = local.subnet_cidrs[each.value]
  availability_zone = each.value

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-private-${each.key}"
    Type = "private"
  })
}

resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-igw"
  })
}

resource "aws_eip" "nat" {
  count  = local.az_count
  domain = "vpc"

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-nat-eip-${count.index + 1}"
  })

  depends_on = [aws_internet_gateway.main]
}

resource "aws_nat_gateway" "main" {
  count = local.az_count

  allocation_id = aws_eip.nat[count.index].id
  subnet_id     = aws_subnet.public[count.index].id

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-nat-${count.index + 1}"
  })
}

# Security Groups
resource "aws_security_group" "web" {
  name        = "${local.name_prefix}-web-sg"
  description = "Security group for web servers"
  vpc_id      = aws_vpc.main.id

  # Dynamic blocks for ingress rules
  dynamic "ingress" {
    for_each = var.web_ingress_rules
    content {
      from_port   = ingress.value.from_port
      to_port     = ingress.value.to_port
      protocol    = ingress.value.protocol
      cidr_blocks = ingress.value.cidr_blocks
      description = ingress.value.description
    }
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
    description = "Allow all outbound traffic"
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-web-sg"
  })

  lifecycle {
    create_before_destroy = true
  }
}

# EC2 Instances
resource "aws_instance" "web" {
  count = var.instance_count

  ami                    = data.aws_ami.amazon_linux.id
  instance_type          = local.instance_type
  subnet_id              = aws_subnet.private[var.availability_zones[count.index % local.az_count]].id
  vpc_security_group_ids = [aws_security_group.web.id]
  key_name               = var.key_name
  iam_instance_profile   = aws_iam_instance_profile.ec2.name

  root_block_device {
    volume_type           = "gp3"
    volume_size           = 50
    encrypted             = true
    delete_on_termination = true

    tags = merge(local.common_tags, {
      Name = "${local.name_prefix}-web-${count.index + 1}-root"
    })
  }

  ebs_block_device {
    device_name           = "/dev/sdf"
    volume_type           = "gp3"
    volume_size           = 100
    encrypted             = true
    delete_on_termination = false
  }

  user_data = base64encode(templatefile("${path.module}/templates/user_data.sh", {
    environment = var.environment
    region      = var.aws_region
    index       = count.index
  }))

  metadata_options {
    http_endpoint               = "enabled"
    http_tokens                 = "required"
    http_put_response_hop_limit = 1
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-web-${count.index + 1}"
    Role = "web"
  })

  lifecycle {
    ignore_changes = [ami, user_data]
  }

  depends_on = [aws_nat_gateway.main]
}

# IAM Resources
resource "aws_iam_role" "ec2" {
  name               = "${local.name_prefix}-ec2-role"
  assume_role_policy = data.aws_iam_policy_document.assume_role.json
  description        = "IAM role for EC2 instances"

  tags = local.common_tags
}

resource "aws_iam_role_policy_attachment" "ssm" {
  role       = aws_iam_role.ec2.name
  policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
}

resource "aws_iam_instance_profile" "ec2" {
  name = "${local.name_prefix}-ec2-profile"
  role = aws_iam_role.ec2.name
}

# S3 Bucket
resource "aws_s3_bucket" "data" {
  bucket = "${local.name_prefix}-data-${data.aws_caller_identity.current.account_id}"

  tags = local.common_tags
}

resource "aws_s3_bucket_versioning" "data" {
  bucket = aws_s3_bucket.data.id

  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "data" {
  bucket = aws_s3_bucket.data.id

  rule {
    apply_server_side_encryption_by_default {
      sse_algorithm     = "aws:kms"
      kms_master_key_id = aws_kms_key.main.arn
    }
    bucket_key_enabled = true
  }
}

resource "aws_s3_bucket_public_access_block" "data" {
  bucket = aws_s3_bucket.data.id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

# KMS Key
resource "aws_kms_key" "main" {
  description             = "KMS key for ${local.name_prefix}"
  deletion_window_in_days = 7
  enable_key_rotation     = true

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "Enable IAM User Permissions"
        Effect = "Allow"
        Principal = {
          AWS = "arn:aws:iam::${data.aws_caller_identity.current.account_id}:root"
        }
        Action   = "kms:*"
        Resource = "*"
      }
    ]
  })

  tags = local.common_tags
}

resource "aws_kms_alias" "main" {
  name          = "alias/${local.name_prefix}"
  target_key_id = aws_kms_key.main.key_id
}

# RDS Database
resource "aws_db_instance" "main" {
  identifier = "${local.name_prefix}-db"

  engine               = var.database_config.engine
  engine_version       = var.database_config.engine_version
  instance_class       = var.database_config.instance_class
  allocated_storage    = var.database_config.allocated_storage
  max_allocated_storage = var.database_config.allocated_storage * 2

  db_name  = "appdb"
  username = var.database_config.username
  password = var.db_password

  multi_az               = var.database_config.multi_az
  db_subnet_group_name   = aws_db_subnet_group.main.name
  vpc_security_group_ids = [aws_security_group.db.id]

  storage_encrypted   = true
  kms_key_id          = aws_kms_key.main.arn
  storage_type        = "gp3"

  backup_retention_period = 7
  backup_window           = "03:00-04:00"
  maintenance_window      = "Mon:04:00-Mon:05:00"

  skip_final_snapshot       = var.environment != "prod"
  final_snapshot_identifier = var.environment == "prod" ? "${local.name_prefix}-final-snapshot" : null

  performance_insights_enabled    = true
  performance_insights_kms_key_id = aws_kms_key.main.arn

  enabled_cloudwatch_logs_exports = ["postgresql", "upgrade"]

  tags = local.common_tags

  lifecycle {
    prevent_destroy = false  # Set to true in production
  }
}

# Secrets Manager
resource "aws_secretsmanager_secret" "db" {
  name        = "${local.name_prefix}/db-credentials"
  description = "Database credentials for ${local.name_prefix}"
  kms_key_id  = aws_kms_key.main.arn

  tags = local.common_tags
}

resource "aws_secretsmanager_secret_version" "db" {
  secret_id = aws_secretsmanager_secret.db.id

  secret_string = jsonencode({
    username = var.database_config.username
    password = var.db_password
    host     = aws_db_instance.main.endpoint
    port     = aws_db_instance.main.port
    database = aws_db_instance.main.db_name
  })
}

# Load Balancer
resource "aws_lb" "main" {
  name               = "${local.name_prefix}-alb"
  internal           = false
  load_balancer_type = "application"
  security_groups    = [aws_security_group.alb.id]
  subnets            = aws_subnet.public[*].id

  enable_deletion_protection = var.environment == "prod"
  enable_http2               = true

  access_logs {
    bucket  = aws_s3_bucket.logs.bucket
    prefix  = "alb-logs"
    enabled = true
  }

  tags = local.common_tags
}

resource "aws_lb_target_group" "web" {
  name     = "${local.name_prefix}-web-tg"
  port     = 80
  protocol = "HTTP"
  vpc_id   = aws_vpc.main.id

  health_check {
    enabled             = true
    healthy_threshold   = 2
    unhealthy_threshold = 3
    timeout             = 5
    interval            = 30
    path                = "/health"
    matcher             = "200"
  }

  tags = local.common_tags
}

resource "aws_lb_listener" "https" {
  load_balancer_arn = aws_lb.main.arn
  port              = "443"
  protocol          = "HTTPS"
  ssl_policy        = "ELBSecurityPolicy-TLS13-1-2-2021-06"
  certificate_arn   = aws_acm_certificate.main.arn

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.web.arn
  }
}

# Auto Scaling
resource "aws_autoscaling_group" "web" {
  name                = "${local.name_prefix}-asg"
  desired_capacity    = var.instance_count
  max_size            = var.instance_count * 2
  min_size            = 1
  vpc_zone_identifier = [for subnet in aws_subnet.private : subnet.id]
  target_group_arns   = [aws_lb_target_group.web.arn]

  launch_template {
    id      = aws_launch_template.web.id
    version = "$Latest"
  }

  instance_refresh {
    strategy = "Rolling"
    preferences {
      min_healthy_percentage = 90
    }
  }

  tag {
    key                 = "Name"
    value               = "${local.name_prefix}-web"
    propagate_at_launch = true
  }

  dynamic "tag" {
    for_each = local.common_tags
    content {
      key                 = tag.key
      value               = tag.value
      propagate_at_launch = true
    }
  }
}

# Random resources
resource "random_id" "suffix" {
  byte_length = 4
}

resource "random_password" "db" {
  length           = 32
  special          = true
  override_special = "!#$%&*()-_=+[]{}<>:?"
}

resource "random_uuid" "example" {}

# Null resource for provisioners
resource "null_resource" "example" {
  triggers = {
    always_run = timestamp()
  }

  provisioner "local-exec" {
    command = "echo 'Hello from Terraform'"
  }

  provisioner "local-exec" {
    when    = destroy
    command = "echo 'Destroying resource'"
  }
}

# Time-based resources
resource "time_sleep" "wait" {
  depends_on = [aws_instance.web]

  create_duration = "30s"
}

resource "time_rotating" "rotation" {
  rotation_days = 30
}

# ==============================================================================
# Modules
# ==============================================================================

module "vpc" {
  source  = "terraform-aws-modules/vpc/aws"
  version = "5.0.0"

  name = "${local.name_prefix}-vpc"
  cidr = var.vpc_cidr

  azs             = var.availability_zones
  private_subnets = var.private_subnet_cidrs
  public_subnets  = var.public_subnet_cidrs

  enable_nat_gateway     = true
  single_nat_gateway     = var.environment != "prod"
  enable_vpn_gateway     = false
  enable_dns_hostnames   = true
  enable_dns_support     = true

  tags = local.common_tags
}

module "eks" {
  source = "./modules/eks"

  cluster_name    = "${local.name_prefix}-eks"
  cluster_version = "1.28"
  vpc_id          = module.vpc.vpc_id
  subnet_ids      = module.vpc.private_subnets

  node_groups = {
    default = {
      instance_types = ["t3.medium"]
      desired_size   = 2
      min_size       = 1
      max_size       = 5
    }
  }

  tags = local.common_tags

  providers = {
    aws = aws
  }
}

# ==============================================================================
# Outputs
# ==============================================================================

output "vpc_id" {
  description = "ID of the VPC"
  value       = aws_vpc.main.id
}

output "public_subnet_ids" {
  description = "IDs of public subnets"
  value       = aws_subnet.public[*].id
}

output "private_subnet_ids" {
  description = "IDs of private subnets"
  value       = [for subnet in aws_subnet.private : subnet.id]
}

output "instance_ids" {
  description = "IDs of EC2 instances"
  value       = aws_instance.web[*].id
}

output "load_balancer_dns" {
  description = "DNS name of the load balancer"
  value       = aws_lb.main.dns_name
}

output "database_endpoint" {
  description = "Endpoint of the RDS database"
  value       = aws_db_instance.main.endpoint
  sensitive   = true
}

output "all_outputs" {
  description = "Combined outputs"
  value = {
    vpc_id              = aws_vpc.main.id
    alb_dns             = aws_lb.main.dns_name
    instance_count      = length(aws_instance.web)
    environment         = var.environment
    account_id          = data.aws_caller_identity.current.account_id
    region              = data.aws_region.current.name
  }
}

# ==============================================================================
# Built-in Functions Examples
# ==============================================================================

locals {
  # String functions
  upper_env     = upper(var.environment)
  lower_env     = lower(var.environment)
  title_env     = title(var.environment)
  trimmed       = trim("  hello  ", " ")
  trimprefix    = trimprefix("helloworld", "hello")
  trimsuffix    = trimsuffix("helloworld", "world")
  replace_str   = replace("hello-world", "-", "_")
  split_str     = split(",", "a,b,c")
  join_str      = join("-", ["a", "b", "c"])
  format_str    = format("Hello, %s!", "World")
  formatlist    = formatlist("Hello, %s!", ["Alice", "Bob"])
  regex_match   = regex("^[a-z]+", "hello123")
  regexall      = regexall("[a-z]+", "hello world")
  substr_val    = substr("hello", 0, 3)
  chomp_val     = chomp("hello\n")
  indent_val    = indent(4, "line1\nline2")

  # Numeric functions
  abs_val       = abs(-5)
  ceil_val      = ceil(3.2)
  floor_val     = floor(3.8)
  log_val       = log(10, 100)
  max_val       = max(1, 2, 3)
  min_val       = min(1, 2, 3)
  parseint_val  = parseint("FF", 16)
  pow_val       = pow(2, 10)
  signum_val    = signum(-5)

  # Collection functions
  length_val    = length([1, 2, 3])
  element_val   = element(["a", "b", "c"], 1)
  index_val     = index(["a", "b", "c"], "b")
  contains_val  = contains(["a", "b", "c"], "b")
  distinct_val  = distinct([1, 1, 2, 2, 3])
  chunklist_val = chunklist([1, 2, 3, 4, 5], 2)
  flatten_val   = flatten([[1, 2], [3, 4]])
  keys_val      = keys({a = 1, b = 2})
  values_val    = values({a = 1, b = 2})
  lookup_val    = lookup({a = 1, b = 2}, "a", 0)
  merge_val     = merge({a = 1}, {b = 2})
  zipmap_val    = zipmap(["a", "b"], [1, 2])
  reverse_val   = reverse([1, 2, 3])
  sort_val      = sort(["c", "a", "b"])
  range_val     = range(1, 5)
  coalesce_val  = coalesce("", "default")
  coalescelist  = coalescelist([], ["default"])
  compact_val   = compact(["a", "", "b"])
  concat_val    = concat([1, 2], [3, 4])
  slice_val     = slice([1, 2, 3, 4, 5], 1, 3)
  setproduct    = setproduct(["a", "b"], [1, 2])
  setintersection = setintersection([1, 2], [2, 3])
  setunion      = setunion([1, 2], [2, 3])
  setsubtract   = setsubtract([1, 2, 3], [2])
  one_val       = one([1])

  # Type conversion functions
  tostring_val  = tostring(42)
  tonumber_val  = tonumber("42")
  tobool_val    = tobool("true")
  tolist_val    = tolist([1, 2, 3])
  toset_val     = toset([1, 1, 2])
  tomap_val     = tomap({a = 1})

  # Encoding functions
  base64encode  = base64encode("hello")
  base64decode  = base64decode("aGVsbG8=")
  base64gzip    = base64gzip("hello world")
  jsonencode_v  = jsonencode({a = 1, b = 2})
  jsondecode_v  = jsondecode("{\"a\": 1}")
  yamlencode_v  = yamlencode({a = 1})
  yamldecode_v  = yamldecode("a: 1")
  urlencode_v   = urlencode("hello world")
  csvdecode_v   = csvdecode("a,b\n1,2")

  # Hash functions
  md5_val       = md5("hello")
  sha1_val      = sha1("hello")
  sha256_val    = sha256("hello")
  sha512_val    = sha512("hello")
  bcrypt_val    = bcrypt("password")
  uuid_val      = uuid()

  # Filesystem functions
  file_content  = file("${path.module}/files/example.txt")
  fileexists_v  = fileexists("${path.module}/files/example.txt")
  fileset_v     = fileset(path.module, "*.tf")
  filebase64    = filebase64("${path.module}/files/example.txt")
  templatefile  = templatefile("${path.module}/templates/example.tpl", {name = "World"})
  abspath_val   = abspath(path.module)
  dirname_val   = dirname("/path/to/file.txt")
  basename_val  = basename("/path/to/file.txt")
  pathexpand_v  = pathexpand("~/.ssh/id_rsa")

  # Date/time functions
  timestamp_v   = timestamp()
  formatdate_v  = formatdate("YYYY-MM-DD", timestamp())
  timeadd_val   = timeadd(timestamp(), "24h")
  timecmp_val   = timecmp(timestamp(), "2024-01-01T00:00:00Z")

  # IP functions
  cidrhost_val  = cidrhost("10.0.0.0/24", 5)
  cidrnetmask   = cidrnetmask("10.0.0.0/24")
  cidrsubnet_v  = cidrsubnet("10.0.0.0/16", 8, 1)
  cidrsubnets_v = cidrsubnets("10.0.0.0/16", 8, 8, 8)

  # Type checking
  can_val       = can(regex("[a-z]+", "hello"))
  try_val       = try(regex("[a-z]+", "123"), "default")
  type_val      = type("hello")
  sensitive_v   = sensitive("secret")
  nonsensitive  = nonsensitive(sensitive("not-secret"))
}

# ==============================================================================
# End of HCL Sample
# ==============================================================================
