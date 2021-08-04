# Machine Learning for Synthesis in NITTA

ML can be used to improve speed and quality of synthesis methods in NITTA. It is suggested to use ML model to statistically estimate the numerical target function based on training data gathered from a set of precomputed synthesis trees.

![image](https://user-images.githubusercontent.com/5229130/123153468-5dcae080-d46e-11eb-8867-f9c1944ffae4.png)

## Prototype

A proof of concept has been implemented as an external Jupyter notebook. Key features:

- communicating with NITTA using its web API
- synthesis tree depth-first traversal to get training data
- features preprocessing, label calculation
- training of a simple ML model (relatively small neural network)
- model evaluation

### Tech stack

Python 3.7, Pandas for data processing, Tensorflow for ML.

### How to Run

1. Make sure you have Python 3.7 installed or greater.
2. _(optional)_ Create a `virtualenv` if you don't want to flood system interpreter with packages (tensorflow is >1GB though, so you may want not to have multiple instances of it).
3. `cd <repo_root>/ml/prototype`
4. `pip install -r requirements.txt`
5. `jupyter notebook` or `jupyter notebook nitta-ml-for-synthesis-prototype.ipynb`
6. In browser open `nitta-ml-for-synthesis-prototype.ipynb` and have fun :)
7. Smoke test: `jupyter nbconvert --to markdown --execute --ExecutePreprocessor.kernel_name=python3 --stdout nitta-ml-for-synthesis-prototype.ipynb`
