# qclc
Build quality control limits chart

[Theoretical Background](#theoretical-background)><br>


### Theoretical Background

#### Moving Average

#### Exponential Weighted Moving Average

The **EWMA statistics** is given by: 

$$
Z_i = \lambda \sum_{j=0}^{i-1} (1-\lambda)Y_{i-j} + (1-\lambda)^i Z_0
$$

Where:

-  $Z_i$ is the EWMA statistic.
-  $Z_0$ is the process mean.
-  $Y$ is the variable for which to calculate the EWMA statistic.
-  $\lambda$ is the weighting factor for EWMA.

<br>

The **Estimate of the Process Variance** is given by:

$$
\sigma^2(Z_i) = [(1-(1-\lambda)^{2i})\frac{{\lambda}}{{2-\lambda}}]\sigma^2_Y
$$

Where:

-  $\sigma^2(Z_i)$ is the estimated variance of the EWMA statistic.
-  $\sigma^2_Y$ is the variance of the sample.
-  $\lambda$ is weighting factor for EWMA.
