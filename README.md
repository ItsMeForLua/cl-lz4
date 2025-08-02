# cl-lz4

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Quicklisp](https://img.shields.io/badge/Quicklisp-available-brightgreen.svg)](http://quicklisp.org/)

A pure Common Lisp implementation of the LZ4 compression algorithm.

- A much faster variant can be found [here.](https://github.com/ItsMeForLua/cl-lz4-rs.git)

## Overview

`cl-lz4` is a from-scratch implementation of the LZ4 block format, written entirely in Common Lisp. It is designed for performance, portability, and ease of use, providing a fast and dependency-free compression solution for Lisp applications.

While FFI wrappers can be faster, this library provides a pure Lisp alternative that requires no C libraries or Rust toolchains, making it exceptionally portable and easy to integrate into any Common Lisp project.

## Features

- **Pure Common Lisp:** No external C or Rust dependencies. Works anywhere you have a Lisp implementation.
  
- **High Performance:** Optimized for speed, outperforming many other pure Lisp compression libraries.
  
- **Cross-Platform:** Works on Linux, macOS, and Windows.
  
- **Full CI/CD Setup:** Includes a complete testing and benchmarking pipeline using Docker and Jenkins.
  

## Performance

For a pure Lisp library, `cl-lz4` offers competitive performance, especially when compared to other standard compression libraries like Zlib (via Salza2/Chipz). It provides a significant speed advantage, making it a great choice when FFI is not desirable.

**50MB Data Compression/Decompression Benchmark:**

| **Library** | **Operation** | **Avg. Throughput** |
| --- | --- | --- |
| **cl-lz4** | **Decompression** | **~250 MB/s** |
| Chipz (zlib) | Decompression | ~140 MB/s |
|     |     |     |
| **cl-lz4** | **Compression** | **~150 MB/s** |
| Salza2 (zlib) | Compression | ~75 MB/s |

*Your results may vary based on hardware*

## Installation (for Users)

Once available in Quicklisp, you can load it with:

``` lisp
(ql:quickload :cl-lz4)
```

Until then, clone the repository into your Quicklisp `local-projects` directory:

``` bash
cd ~/quicklisp/local-projects/
git clone https://github.com/ItsMeForLua/cl-lz4.git
```

Then, load the system in your REPL:

``` lisp
(ql:quickload :cl-lz4)
```
## Usage Example

Reference the functions in your own code. All symbols are exported from the `cl-lz4` package.

```  lisp
(use-package :cl-lz4)

;; Create some sample data
(defparameter *original-data*
  (let ((vec (make-array 1024 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length vec)
          do (setf (aref vec i) (mod i 256)))
    vec))

(defparameter *compressed* (compress *original-data*))
(defparameter *decompressed* (decompress *compressed* :uncompressed-size (length *original-data*)))

;; Verify the round-trip was successful
(format t "Round-trip successful: ~A~%" (equalp *original-data* *decompressed*))
;; => Round-trip successful: T
```

---

## Developer Setup (for Contributors)

This project uses **Qlot** for reproducible dependency management and a **Makefile** for automating common tasks.

1. **Prerequisites**:
  
  - Install [Roswell](https://github.com/roswell/roswell/wiki "null") (for managing Lisp implementations).
    
  - Install [Qlot](https://github.com/fukamachi/qlot "null") (`ros install qlot`).
    
  - Install [Docker](https://www.docker.com/ "null") and [Docker Compose](https://docs.docker.com/compose/install/ "null") (for running the CI pipeline).
    
### Dependency Management with Qlot

1. **Install Project Dependencies**: This command reads the `qlfile` and installs the exact versions of all dependencies into a local `.qlot/` directory.
  
  ``` bash
  # From the project's root directory
  make deps
  ```
  
2. **Start a REPL**: To work on the project, start your REPL using `qlot exec`. This ensures your Lisp session uses the project's local dependencies.
  
  ``` bash
  qlot exec ros run
  ```

### Continuous Integration with Docker & Jenkins

The repository includes a complete CI setup to automate testing and ensure code quality.

- **`Dockerfile`**: Defines a build environment with Roswell, SBCL, Qlot, and the Rust toolchain pre-installed.
  
- **`docker-compose.yml` & `Dockerfile.jenkins`**: These files set up a local Jenkins instance, configured to run the project's pipeline.
  
- **`Jenkinsfile`**: This file defines the CI pipeline stages: build, test, and benchmark.
  

To run the full CI pipeline locally:

1. **Start the Jenkins server**:
  
  ``` bash
  # Ensure your user can access the Docker daemon
  docker-compose up -d
  ```
  
2. **Access Jenkins** at `http://localhost:8080`.
  
3. **Create a new "Pipeline" job** and configure it to use the `Jenkinsfile` from SCM (pointing to your local git repository). This will replicate the exact environment used for automated testing.

## Running Tests and Benchmarks

The `Makefile` provides the easiest way to run the test and benchmark suites.

- **To run the test suite**:
  
  ``` bash
  # Using the qlot environment
  make test
  
  # Or using your local system's Lisp environment (works offline)
  make test-local
  ```
  
- **To run the benchmarks**:
  
  ``` bash
  # Using the qlot environment
  make benchmark
  
  # Or using your local system's Lisp environment
  make benchmark-local
  ```

## Contributing

Bug reports and pull requests are welcome on GitHub. Please ensure the test suite passes by atleast running `make test` before submitting a pull request. However, it is preferable you use the Jenkins CI/CD pipeline before submitting a PR, as this will prevent silly mistakes (happens to all of us), and will reduce the Github actions usage

## License

This project is licensed under the **MIT License**. See the `LICENSE` file for details.