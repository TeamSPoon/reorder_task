
from setuptools import setup

setup(
    name="reorder_task",
    version="0.1",
    py_modules=["metta_to_prolog"],
    package_dir={"": "."},
    install_requires=["openai"],
    entry_points={
        "console_scripts": [
            "metta2prolog=metta_to_prolog:main"
        ]
    }
)
