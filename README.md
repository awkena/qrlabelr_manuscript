<!-- badges: start -->
[![DOI](https://zenodo.org/badge/734320211.svg)](https://zenodo.org/doi/10.5281/zenodo.10636680)
Microbenchmarking data and scripts for testing the hypothesis that the QR code generation functionality in qrlabelr is faster than its counterpart in the baRcodeR package.
<!-- badges: end -->

Fig. 4. Microbenchmarking the QR code generation functions in qrlabelr and baRcodeR packages

Generating data to test the hypothesis that the QR code generation functionality in the qrlabelr pckage is faster than its counterpart in the baRocdeR package

We microbenchmarked the two functions on a PC and a MacBook with following specs:
### Windows HP PC (8 cores)
- Processor:	Intel(R) Core(TM) i7-10510U CPU @ 1.80GHz   2.30 GHz
- Installed RAM:	16.0 GB (15.7 GB usable)
- System type:	64-bit operating system, x64-based processor

### MacBook Pro (12 cores)
- Chip: Apple M2 Max
- Memory: 32 GB
- macOS Sonoma 14.2
- Platform: aarch64-apple-darwin20 (64-bit)

We generated microbenchmarking data independently on both computers and saved the data for results summary and plotting on the MacBook Pro.

We saved the data as follows:
- MacBook Pro: time_mac.csv
- Windows PC: time_win.csv
