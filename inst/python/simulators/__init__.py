"""
ANCOVA Simulators for BayesFlow Training and Inference

These simulators generate trial data in batch format compatible with
BayesFlow neural posterior estimation. They mirror the R batch simulation
functions in rctbayespower::models_ancova.R.

Usage from R:
    py_sims <- reticulate::import_from_path(
        "simulators",
        system.file("python", package = "rctbayespower")
    )
    data = py_sims$simulate_ancova_2arms(n_sims=64, n_total=100, b_arm_treat=0.5)
"""

from .ancova import (
    simulate_ancova_2arms,
    simulate_ancova_3arms,
    ANCOVASimulator2Arms,
    ANCOVASimulator3Arms,
)

__all__ = [
    "simulate_ancova_2arms",
    "simulate_ancova_3arms",
    "ANCOVASimulator2Arms",
    "ANCOVASimulator3Arms",
]
