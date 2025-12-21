#!/bin/bash
# =============================================================================
# Azure ML Compute Instance Setup for BayesFlow Training
# =============================================================================
# Mirrors rctbayespower::setup_bf_python() for Azure cloud training.
#
# Usage:
#   ./setup_azure_bf.sh --subscription-id SUB --resource-group RG --workspace WS
#   ./setup_azure_bf.sh --help
#
# Prerequisites:
#   - Azure CLI installed (az)
#   - Logged in: az login
#   - ML extension: az extension add -n ml

set -e

# =============================================================================
# DEFAULTS
# =============================================================================

DEFAULT_VM_SIZE="Standard_NC4as_T4_v3"
DEFAULT_COMPUTE_NAME="bf-training"
DEFAULT_IDLE_SHUTDOWN=30
DEFAULT_ENV_NAME="bayesflow-training-env"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# =============================================================================
# COLORS
# =============================================================================

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

info() { echo -e "${BLUE}[INFO]${NC} $1"; }
success() { echo -e "${GREEN}[OK]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

# =============================================================================
# HELP
# =============================================================================

show_help() {
    cat << EOF
Azure ML Compute Instance Setup for BayesFlow Training

USAGE:
    $(basename "$0") [OPTIONS]

REQUIRED:
    --subscription-id ID    Azure subscription ID
    --resource-group RG     Resource group name
    --workspace WS          Azure ML workspace name

OPTIONS:
    --compute-name NAME     Compute instance name (default: $DEFAULT_COMPUTE_NAME)
    --vm-size SIZE          VM size (default: $DEFAULT_VM_SIZE)
    --idle-shutdown MIN     Idle shutdown minutes, 0=disable (default: $DEFAULT_IDLE_SHUTDOWN)
    --env-name NAME         Environment name (default: $DEFAULT_ENV_NAME)

ACTIONS:
    --env-only              Only create environment, skip compute
    --compute-only          Only create compute, skip environment
    --delete                Delete the compute instance
    --list-vm-sizes         List available GPU VM sizes
    --help                  Show this help

EXAMPLES:
    # Create compute instance with defaults
    $(basename "$0") --subscription-id SUB --resource-group RG --workspace WS

    # Use V100 GPU instead of T4
    $(basename "$0") --subscription-id SUB --resource-group RG --workspace WS \\
        --vm-size Standard_NC6s_v3

    # Delete compute instance
    $(basename "$0") --subscription-id SUB --resource-group RG --workspace WS --delete

GPU VM SIZES:
    Standard_NC4as_T4_v3    1x T4 (16GB)   ~\$0.50/hr  [DEFAULT]
    Standard_NC6s_v3        1x V100 (16GB) ~\$3.00/hr
    Standard_NC12s_v3       2x V100 (32GB) ~\$6.00/hr
    Standard_NC24s_v3       4x V100 (64GB) ~\$13.00/hr
EOF
}

list_vm_sizes() {
    echo ""
    echo "Available GPU VM Sizes:"
    echo "----------------------------------------------------------------------"
    printf "%-25s %-12s %-8s %-12s\n" "VM Size" "GPU" "VRAM" "Cost/hr"
    echo "----------------------------------------------------------------------"
    printf "%-25s %-12s %-8s %-12s\n" "Standard_NC4as_T4_v3" "1x T4" "16GB" "~\$0.50"
    printf "%-25s %-12s %-8s %-12s\n" "Standard_NC8as_T4_v3" "1x T4" "16GB" "~\$0.75"
    printf "%-25s %-12s %-8s %-12s\n" "Standard_NC6s_v3" "1x V100" "16GB" "~\$3.00"
    printf "%-25s %-12s %-8s %-12s\n" "Standard_NC12s_v3" "2x V100" "32GB" "~\$6.00"
    printf "%-25s %-12s %-8s %-12s\n" "Standard_NC24s_v3" "4x V100" "64GB" "~\$13.00"
    echo "----------------------------------------------------------------------"
    echo "Default: $DEFAULT_VM_SIZE"
    echo ""
}

# =============================================================================
# PARSE ARGUMENTS
# =============================================================================

SUBSCRIPTION_ID=""
RESOURCE_GROUP=""
WORKSPACE=""
COMPUTE_NAME="$DEFAULT_COMPUTE_NAME"
VM_SIZE="$DEFAULT_VM_SIZE"
IDLE_SHUTDOWN="$DEFAULT_IDLE_SHUTDOWN"
ENV_NAME="$DEFAULT_ENV_NAME"
ENV_ONLY=false
COMPUTE_ONLY=false
DELETE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --subscription-id) SUBSCRIPTION_ID="$2"; shift 2 ;;
        --resource-group) RESOURCE_GROUP="$2"; shift 2 ;;
        --workspace) WORKSPACE="$2"; shift 2 ;;
        --compute-name) COMPUTE_NAME="$2"; shift 2 ;;
        --vm-size) VM_SIZE="$2"; shift 2 ;;
        --idle-shutdown) IDLE_SHUTDOWN="$2"; shift 2 ;;
        --env-name) ENV_NAME="$2"; shift 2 ;;
        --env-only) ENV_ONLY=true; shift ;;
        --compute-only) COMPUTE_ONLY=true; shift ;;
        --delete) DELETE=true; shift ;;
        --list-vm-sizes) list_vm_sizes; exit 0 ;;
        --help|-h) show_help; exit 0 ;;
        *) error "Unknown option: $1. Use --help for usage." ;;
    esac
done

# =============================================================================
# VALIDATION
# =============================================================================

if [[ -z "$SUBSCRIPTION_ID" ]] || [[ -z "$RESOURCE_GROUP" ]] || [[ -z "$WORKSPACE" ]]; then
    error "Required: --subscription-id, --resource-group, --workspace. Use --help for usage."
fi

# Check Azure CLI
if ! command -v az &> /dev/null; then
    error "Azure CLI (az) not found. Install from: https://aka.ms/installazurecli"
fi

# Check ML extension
if ! az extension show -n ml &> /dev/null; then
    info "Installing Azure ML CLI extension..."
    az extension add -n ml -y
fi

# Set subscription
info "Setting subscription: $SUBSCRIPTION_ID"
az account set --subscription "$SUBSCRIPTION_ID"

# =============================================================================
# DELETE ACTION
# =============================================================================

if [[ "$DELETE" == true ]]; then
    info "Deleting compute instance: $COMPUTE_NAME"
    az ml compute delete \
        --name "$COMPUTE_NAME" \
        --resource-group "$RESOURCE_GROUP" \
        --workspace-name "$WORKSPACE" \
        --yes || warn "Compute instance not found or already deleted"
    success "Done"
    exit 0
fi

# =============================================================================
# CREATE ENVIRONMENT
# =============================================================================

if [[ "$COMPUTE_ONLY" != true ]]; then
    info "Creating environment: $ENV_NAME"

    # Check if Dockerfile exists
    DOCKERFILE="$SCRIPT_DIR/Dockerfile.bayesflow"
    if [[ ! -f "$DOCKERFILE" ]]; then
        error "Dockerfile not found: $DOCKERFILE"
    fi

    # Create environment YAML
    ENV_YAML=$(mktemp)
    cat > "$ENV_YAML" << EOF
\$schema: https://azuremlschemas.azureedge.net/latest/environment.schema.json
name: $ENV_NAME
build:
  path: $SCRIPT_DIR
  dockerfile_path: Dockerfile.bayesflow
description: BayesFlow 2.0 training environment with PyTorch/CUDA
EOF

    az ml environment create \
        --file "$ENV_YAML" \
        --resource-group "$RESOURCE_GROUP" \
        --workspace-name "$WORKSPACE" \
        || warn "Environment may already exist"

    rm -f "$ENV_YAML"
    success "Environment created: $ENV_NAME"
fi

# =============================================================================
# CREATE COMPUTE INSTANCE
# =============================================================================

if [[ "$ENV_ONLY" != true ]]; then
    info "Creating compute instance: $COMPUTE_NAME (VM: $VM_SIZE)"

    # Check if compute exists
    if az ml compute show \
        --name "$COMPUTE_NAME" \
        --resource-group "$RESOURCE_GROUP" \
        --workspace-name "$WORKSPACE" &> /dev/null; then
        warn "Compute instance '$COMPUTE_NAME' already exists"
    else
        # Create compute YAML
        COMPUTE_YAML=$(mktemp)
        cat > "$COMPUTE_YAML" << EOF
\$schema: https://azuremlschemas.azureedge.net/latest/computeInstance.schema.json
name: $COMPUTE_NAME
type: computeinstance
size: $VM_SIZE
idle_time_before_shutdown_minutes: $IDLE_SHUTDOWN
EOF

        az ml compute create \
            --file "$COMPUTE_YAML" \
            --resource-group "$RESOURCE_GROUP" \
            --workspace-name "$WORKSPACE"

        rm -f "$COMPUTE_YAML"
        success "Compute instance created: $COMPUTE_NAME"
    fi

    # Show compute info
    echo ""
    info "Compute details:"
    echo "  Name: $COMPUTE_NAME"
    echo "  VM Size: $VM_SIZE"
    echo "  Idle shutdown: ${IDLE_SHUTDOWN} minutes"
fi

# =============================================================================
# SUMMARY
# =============================================================================

echo ""
success "Setup complete!"
echo ""
echo "Next steps:"
echo "  1. Open Azure ML Studio: https://ml.azure.com"
echo "  2. Navigate to Compute > Compute instances"
echo "  3. Connect to '$COMPUTE_NAME' via JupyterLab or terminal"
echo "  4. Upload training scripts and run"
echo ""
echo "To delete the compute instance:"
echo "  $(basename "$0") --subscription-id $SUBSCRIPTION_ID --resource-group $RESOURCE_GROUP --workspace $WORKSPACE --delete"
