# qclc
Build quality control limits chart

[Theoretical Background](#theoretical-background)><br>


### Theoretical Background

#### Moving Average

#### Exponential Weighted Moving Average

The **EWMA statistic** is given by: 

$$
Z_i = \lambda \sum_{j=0}^{i-1} (1-\lambda)Y_{i-j} + (1-\lambda)^i Z_0
$$

Where:

-  $Z_i$ is the EWMA statistic.
-  $Z_0$ is the process mean.
-  $Y$ is the individual observations obtained from the process.
-  $\lambda$ is the weighting factor for EWMA.

<br>

The **Estimate of the Process Variance** is given by:

$$
\sigma^2(Z_i) = [(1-(1-\lambda)^{2i})\frac{{\lambda}}{{2-\lambda}}]\sigma^2_Y
$$

Where:

-  $\sigma^2(Z_i)$ is the estimated variance of the EWMA statistic.
-  $\sigma^2_Y$ is the variance of the original sample.
-  $\lambda$ is the weighting factor for EWMA.

The **Upper Control Limit** for EWMA is given by: 

$$
\UCL(Z_i) = Z_0 + \L \sigma^2(Z_i)
$$

Where:

- $Z_0$ is the process mean.
- $L$ is the Control Limit Factor.
- $\sigma^2(Z_i)$ is the estimate of the process Variance.
