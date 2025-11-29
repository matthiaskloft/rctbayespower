"""
ANCOVA Simulators for 2-arm and 3-arm Trials

These functions mirror the R batch simulation functions:
- simulate_data_ancova_cont_2arms_batch()
- simulate_data_ancova_cont_3arms_batch()

Output format matches what BayesFlow models expect:
- outcome: (n_sims, n_total) array
- covariate: (n_sims, n_total) array
- group: (n_sims, n_total) array (binary for 2-arm, 0/1/2 for 3-arm)
"""

import numpy as np
from typing import Dict, Union, Tuple, Optional
from numpy.typing import NDArray


def simulate_ancova_2arms(
    n_sims: int,
    n_total: int,
    p_alloc: float = 0.5,
    intercept: float = 0.0,
    b_arm_treat: float = 0.0,
    b_covariate: float = 0.0,
    sigma: float = 1.0,
    rng: Optional[np.random.Generator] = None,
) -> Dict[str, Union[NDArray, int, float]]:
    """
    Simulate batched ANCOVA data for 2-arm trials.

    Equivalent to R function: simulate_data_ancova_cont_2arms_batch()

    Parameters
    ----------
    n_sims : int
        Number of simulations (batch size)
    n_total : int
        Sample size per simulation
    p_alloc : float, default=0.5
        Probability of treatment allocation
    intercept : float, default=0.0
        Intercept value
    b_arm_treat : float, default=0.0
        Treatment effect coefficient
    b_covariate : float, default=0.0
        Covariate effect coefficient
    sigma : float, default=1.0
        Residual standard deviation
    rng : np.random.Generator, optional
        Random number generator for reproducibility

    Returns
    -------
    dict
        Dictionary with keys:
        - 'outcome': (n_sims, n_total) array
        - 'covariate': (n_sims, n_total) array
        - 'group': (n_sims, n_total) array (0=control, 1=treatment)
        - 'N': int, sample size
        - 'p_alloc': float, allocation probability
    """
    if rng is None:
        rng = np.random.default_rng()

    # Generate covariates
    covariate = rng.standard_normal((n_sims, n_total))

    # Generate treatment assignments
    group = rng.binomial(1, p_alloc, size=(n_sims, n_total))

    # Generate outcomes: Y = intercept + covariate*b_cov + group*b_treat + error
    error = rng.normal(0, sigma, size=(n_sims, n_total))
    outcome = (
        intercept
        + covariate * b_covariate
        + group * b_arm_treat
        + error
    )

    return {
        'outcome': outcome.astype(np.float32),
        'covariate': covariate.astype(np.float32),
        'group': group.astype(np.float32),
        'N': n_total,
        'p_alloc': p_alloc,
    }


def simulate_ancova_3arms(
    n_sims: int,
    n_total: int,
    p_alloc: Tuple[float, float, float] = (1/3, 1/3, 1/3),
    intercept: float = 0.0,
    b_arm_treat: Tuple[float, float] = (0.0, 0.0),
    b_covariate: float = 0.0,
    sigma: float = 1.0,
    rng: Optional[np.random.Generator] = None,
) -> Dict[str, Union[NDArray, int, Tuple]]:
    """
    Simulate batched ANCOVA data for 3-arm trials.

    Equivalent to R function: simulate_data_ancova_cont_3arms_batch()

    Parameters
    ----------
    n_sims : int
        Number of simulations (batch size)
    n_total : int
        Sample size per simulation
    p_alloc : tuple of 3 floats, default=(1/3, 1/3, 1/3)
        Allocation probabilities for (control, treat_1, treat_2)
    intercept : float, default=0.0
        Intercept value
    b_arm_treat : tuple of 2 floats, default=(0.0, 0.0)
        Treatment effect coefficients for treat_1 and treat_2
    b_covariate : float, default=0.0
        Covariate effect coefficient
    sigma : float, default=1.0
        Residual standard deviation
    rng : np.random.Generator, optional
        Random number generator for reproducibility

    Returns
    -------
    dict
        Dictionary with keys:
        - 'outcome': (n_sims, n_total) array
        - 'covariate': (n_sims, n_total) array
        - 'group': (n_sims, n_total) array (0=control, 1=treat_1, 2=treat_2)
        - 'N': int, sample size
        - 'p_alloc': tuple, allocation probabilities
    """
    if rng is None:
        rng = np.random.default_rng()

    p_alloc = tuple(p_alloc)
    b_arm_treat = tuple(b_arm_treat)

    # Generate covariates
    covariate = rng.standard_normal((n_sims, n_total))

    # Generate treatment assignments (0, 1, 2)
    group = rng.choice([0, 1, 2], size=(n_sims, n_total), p=p_alloc)

    # Calculate treatment effects
    treat_effect = np.zeros((n_sims, n_total), dtype=np.float32)
    treat_effect[group == 1] = b_arm_treat[0]
    treat_effect[group == 2] = b_arm_treat[1]

    # Generate outcomes
    error = rng.normal(0, sigma, size=(n_sims, n_total))
    outcome = (
        intercept
        + covariate * b_covariate
        + treat_effect
        + error
    )

    return {
        'outcome': outcome.astype(np.float32),
        'covariate': covariate.astype(np.float32),
        'group': group.astype(np.float32),
        'N': n_total,
        'p_alloc': p_alloc,
    }


class ANCOVASimulator2Arms:
    """
    BayesFlow-compatible simulator for 2-arm ANCOVA trials.

    Can be used directly with BayesFlow's make_simulator() or as a
    standalone simulator for training and inference.

    Example
    -------
    >>> sim = ANCOVASimulator2Arms(b_arm_treat=0.5, b_covariate=0.3)
    >>> data = sim(n_sims=64, n_total=100)
    >>> data['outcome'].shape
    (64, 100)
    """

    def __init__(
        self,
        p_alloc: float = 0.5,
        intercept: float = 0.0,
        b_arm_treat: float = 0.0,
        b_covariate: float = 0.0,
        sigma: float = 1.0,
    ):
        """
        Initialize simulator with fixed parameters.

        Parameters can be overridden at call time.
        """
        self.p_alloc = p_alloc
        self.intercept = intercept
        self.b_arm_treat = b_arm_treat
        self.b_covariate = b_covariate
        self.sigma = sigma

    def __call__(
        self,
        n_sims: int,
        n_total: int,
        **kwargs
    ) -> Dict[str, Union[NDArray, int, float]]:
        """
        Generate batch of simulated trials.

        Parameters
        ----------
        n_sims : int
            Number of simulations
        n_total : int
            Sample size per simulation
        **kwargs
            Override any default parameters

        Returns
        -------
        dict
            Simulated data in BayesFlow format
        """
        params = {
            'p_alloc': kwargs.get('p_alloc', self.p_alloc),
            'intercept': kwargs.get('intercept', self.intercept),
            'b_arm_treat': kwargs.get('b_arm_treat', self.b_arm_treat),
            'b_covariate': kwargs.get('b_covariate', self.b_covariate),
            'sigma': kwargs.get('sigma', self.sigma),
            'rng': kwargs.get('rng', None),
        }
        return simulate_ancova_2arms(n_sims, n_total, **params)

    def sample_prior(
        self,
        n_sims: int,
        rng: Optional[np.random.Generator] = None,
    ) -> Dict[str, NDArray]:
        """
        Sample parameters from prior distribution.

        Default prior:
        - b_arm_treat ~ Normal(0.3, 0.3) truncated to [-1, 2]
        - b_covariate ~ Normal(0, 0.5)
        - sigma ~ HalfNormal(0.5) + 0.1

        Returns
        -------
        dict
            Dictionary with sampled parameters
        """
        if rng is None:
            rng = np.random.default_rng()

        # Sample treatment effect
        b_arm_treat = rng.normal(0.3, 0.3, size=n_sims)
        b_arm_treat = np.clip(b_arm_treat, -1, 2)

        # Sample covariate effect
        b_covariate = rng.normal(0, 0.5, size=n_sims)

        # Sample sigma (half-normal + offset to avoid 0)
        sigma = np.abs(rng.normal(0, 0.5, size=n_sims)) + 0.1

        return {
            'b_arm_treat': b_arm_treat.astype(np.float32),
            'b_covariate': b_covariate.astype(np.float32),
            'sigma': sigma.astype(np.float32),
        }


class ANCOVASimulator3Arms:
    """
    BayesFlow-compatible simulator for 3-arm ANCOVA trials.

    Similar to ANCOVASimulator2Arms but for 3-arm designs.
    """

    def __init__(
        self,
        p_alloc: Tuple[float, float, float] = (1/3, 1/3, 1/3),
        intercept: float = 0.0,
        b_arm_treat: Tuple[float, float] = (0.0, 0.0),
        b_covariate: float = 0.0,
        sigma: float = 1.0,
    ):
        self.p_alloc = tuple(p_alloc)
        self.intercept = intercept
        self.b_arm_treat = tuple(b_arm_treat)
        self.b_covariate = b_covariate
        self.sigma = sigma

    def __call__(
        self,
        n_sims: int,
        n_total: int,
        **kwargs
    ) -> Dict[str, Union[NDArray, int, Tuple]]:
        params = {
            'p_alloc': kwargs.get('p_alloc', self.p_alloc),
            'intercept': kwargs.get('intercept', self.intercept),
            'b_arm_treat': kwargs.get('b_arm_treat', self.b_arm_treat),
            'b_covariate': kwargs.get('b_covariate', self.b_covariate),
            'sigma': kwargs.get('sigma', self.sigma),
            'rng': kwargs.get('rng', None),
        }
        return simulate_ancova_3arms(n_sims, n_total, **params)

    def sample_prior(
        self,
        n_sims: int,
        rng: Optional[np.random.Generator] = None,
    ) -> Dict[str, NDArray]:
        """Sample parameters from prior distribution for 3-arm trials."""
        if rng is None:
            rng = np.random.default_rng()

        # Sample treatment effects (2 coefficients)
        b_arm_treat_1 = rng.normal(0.3, 0.3, size=n_sims)
        b_arm_treat_1 = np.clip(b_arm_treat_1, -1, 2)

        b_arm_treat_2 = rng.normal(0.5, 0.3, size=n_sims)
        b_arm_treat_2 = np.clip(b_arm_treat_2, -1, 2)

        b_covariate = rng.normal(0, 0.5, size=n_sims)
        sigma = np.abs(rng.normal(0, 0.5, size=n_sims)) + 0.1

        return {
            'b_arm_treat_1': b_arm_treat_1.astype(np.float32),
            'b_arm_treat_2': b_arm_treat_2.astype(np.float32),
            'b_covariate': b_covariate.astype(np.float32),
            'sigma': sigma.astype(np.float32),
        }
