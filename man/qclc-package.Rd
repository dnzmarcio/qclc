\name{qclc}
\alias{qclc}
\docType{package}
\title{qclc: Operating Characteristics for Quality Control Limit Charts}
\description{
qclc is an R package for generating the operating characteristics for Moving Average (MA) and exponentially weighted moving average (EWMA) quality control limit charts. It supports monitoring of both in-control and out-of-control processes.
}
\details{
For detailed examples and workflows, see the vignette:
\code{vignette("qclc-guide")}.
}
\section{Functions}{
\describe{
  \item{\code{\link{control_parm_ma}}}{}
  \item{\code{\link{control_parm_ewma}}}{}
  \item{\code{\link{arl_ma}}}{}
  \item{\code{\link{arl_ewma}}}{}
  \item{\code{\link{ma_statistic}}}{}
  \item{\code{\link{ewma_statistic}}}{}
  \item{\code{\link{lower_limit_ma}}}{}
  \item{\code{\link{upper_limit_ma}}}{}
  \item{\code{\link{lower_limit_ewma}}}{}
  \item{\code{\link{upper_limit_ewma}}}{}
}
}
\author{
Marcio Augusto Diniz \email{Marcio.Diniz@mountsinai.org} \cr
Karni Bedirian \email{karni.bedirian@mountsinai.org}
}
\keyword{package}