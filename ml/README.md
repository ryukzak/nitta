# Machine Learning for Synthesis in NITTA

ML can be used to improve speed and quality of synthesis methods in NITTA. It is suggested to use ML model to statistically estimate the numerical target function based on training data gathered from a set of precomputed synthesis trees.

![image](https://user-images.githubusercontent.com/5229130/123153468-5dcae080-d46e-11eb-8867-f9c1944ffae4.png)

## Prototype

A proof of concept has been implemented as an external Jupyter notebook. Key features:

- communicating with NITTA using its web API
- synthesis tree depth-first traversal to get training data
- features preprocessing, label calculation
- training of a simple ML model (relatively small neural network)
- (WIP) model evaluation

### Tech stack

Python 3.7, Pandas for data processing, Tensorflow for ML.

### How to Run

1. Make sure you have Docker installed.
2. Build Dockerfile with --target dependencies
3. Run built image with bind mount of repo root to /app
4. /bin/bash into container to execute whatever you want
5. Consider using --network host not to care about connectivity as well 

TODO: specific commands and docker-compose to automate part of them
