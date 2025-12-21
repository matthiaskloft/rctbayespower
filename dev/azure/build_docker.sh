#!/bin/bash
# =============================================================================
# Build and Run BayesFlow Docker Image
# =============================================================================
# Builds the Docker image for local GPU training.
#
# Usage:
#   ./build_docker.sh           # Build image
#   ./build_docker.sh --run     # Build and run with GPU
#   ./build_docker.sh --run-cpu # Build and run without GPU
#   ./build_docker.sh --push    # Build and push to registry

set -e

# =============================================================================
# CONFIGURATION
# =============================================================================

IMAGE_NAME="bayesflow-training"
IMAGE_TAG="latest"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOCKERFILE="$SCRIPT_DIR/Dockerfile.bayesflow"

# =============================================================================
# COLORS
# =============================================================================

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

info() { echo -e "${BLUE}[INFO]${NC} $1"; }
success() { echo -e "${GREEN}[OK]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

# =============================================================================
# HELP
# =============================================================================

show_help() {
    cat << EOF
Build and Run BayesFlow Docker Image

USAGE:
    $(basename "$0") [OPTIONS]

OPTIONS:
    --run           Build and run with GPU support
    --run-cpu       Build and run without GPU (CPU only)
    --push          Build and push to container registry
    --registry URL  Container registry URL (for --push)
    --tag TAG       Image tag (default: latest)
    --no-cache      Build without Docker cache
    --help          Show this help

EXAMPLES:
    # Just build the image
    $(basename "$0")

    # Build and run interactively with GPU
    $(basename "$0") --run

    # Build and push to Azure Container Registry
    $(basename "$0") --push --registry myregistry.azurecr.io

REQUIREMENTS:
    - Docker installed
    - NVIDIA Container Toolkit (for GPU support)
    - nvidia-smi working (for GPU support)
EOF
}

# =============================================================================
# PARSE ARGUMENTS
# =============================================================================

RUN_GPU=false
RUN_CPU=false
PUSH=false
REGISTRY=""
NO_CACHE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --run) RUN_GPU=true; shift ;;
        --run-cpu) RUN_CPU=true; shift ;;
        --push) PUSH=true; shift ;;
        --registry) REGISTRY="$2"; shift 2 ;;
        --tag) IMAGE_TAG="$2"; shift 2 ;;
        --no-cache) NO_CACHE="--no-cache"; shift ;;
        --help|-h) show_help; exit 0 ;;
        *) error "Unknown option: $1" ;;
    esac
done

# =============================================================================
# VALIDATION
# =============================================================================

if ! command -v docker &> /dev/null; then
    error "Docker not found. Install from: https://docs.docker.com/get-docker/"
fi

if [[ ! -f "$DOCKERFILE" ]]; then
    error "Dockerfile not found: $DOCKERFILE"
fi

# =============================================================================
# BUILD
# =============================================================================

FULL_IMAGE="$IMAGE_NAME:$IMAGE_TAG"

info "Building Docker image: $FULL_IMAGE"
docker build \
    $NO_CACHE \
    -t "$FULL_IMAGE" \
    -f "$DOCKERFILE" \
    "$SCRIPT_DIR"

success "Image built: $FULL_IMAGE"

# =============================================================================
# PUSH
# =============================================================================

if [[ "$PUSH" == true ]]; then
    if [[ -z "$REGISTRY" ]]; then
        error "Registry required for --push. Use --registry URL"
    fi

    REMOTE_IMAGE="$REGISTRY/$FULL_IMAGE"
    info "Tagging for registry: $REMOTE_IMAGE"
    docker tag "$FULL_IMAGE" "$REMOTE_IMAGE"

    info "Pushing to registry..."
    docker push "$REMOTE_IMAGE"
    success "Pushed: $REMOTE_IMAGE"
fi

# =============================================================================
# RUN
# =============================================================================

if [[ "$RUN_GPU" == true ]]; then
    # Check for NVIDIA runtime
    if ! docker info 2>/dev/null | grep -q "nvidia"; then
        warn "NVIDIA runtime not detected. GPU may not work."
        warn "Install NVIDIA Container Toolkit: https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/install-guide.html"
    fi

    info "Running with GPU support..."
    docker run \
        --gpus all \
        -it \
        --rm \
        -v "$(pwd):/workspace" \
        -e KERAS_BACKEND=torch \
        "$FULL_IMAGE"

elif [[ "$RUN_CPU" == true ]]; then
    info "Running in CPU-only mode..."
    docker run \
        -it \
        --rm \
        -v "$(pwd):/workspace" \
        -e KERAS_BACKEND=torch \
        "$FULL_IMAGE"
fi

# =============================================================================
# SUMMARY
# =============================================================================

if [[ "$RUN_GPU" != true ]] && [[ "$RUN_CPU" != true ]]; then
    echo ""
    success "Build complete!"
    echo ""
    echo "To run with GPU:"
    echo "  docker run --gpus all -it -v \$(pwd):/workspace $FULL_IMAGE"
    echo ""
    echo "To run CPU-only:"
    echo "  docker run -it -v \$(pwd):/workspace $FULL_IMAGE"
    echo ""
    echo "Or use this script:"
    echo "  $(basename "$0") --run      # GPU"
    echo "  $(basename "$0") --run-cpu  # CPU"
fi
