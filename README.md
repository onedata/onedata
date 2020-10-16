
![Onedata](resources/logo.png)

This is the main code repository of [Onedata](http://onedata.org) - a global data management system, providing easy access to distributed storage resources, supporting wide range of use cases from personal data management to data-intensive scientific computations.

**Onedata** is composed of several components:

  * [Onezone](https://onedata.org/docs/doc/administering_onedata/onezone_overview.html) - allows to connect multiple storage providers into a larger distributed domain and provides users with Graphical User Interface for typical data management tasks,
  * [Oneprovider](https://onedata.org/docs/doc/administering_onedata/provider_overview.html) - the main data management component of Onedata, deployed at each storage provider site, responsible for unifying and controlling access to data over low level storage resources of the provider,
  * [Oneclient](https://onedata.org/docs/doc/using_onedata/oneclient.html) - command line tool which enables transparent access to users data spaces through [Fuse](https://github.com/libfuse/libfuse) virtual filesystem,
  * [Onepanel](https://onedata.org/docs/doc/administering_onedata/onepanel_overview.html) - administration and configuration interface for **Onezone** and **Oneprovider** components,
  * [LUMA](https://onedata.org/docs/doc/administering_onedata/luma.html) - service which allows mapping of between Onedata user accounts and local storage ID's, here we provide an example implementation of this service.

This repository combines these components into one source package, which can be used to build and test complete Onedata platform. Each of the components consists of the following submodules of this repository:

## Common components

| Submodule | URL      | Description |
|----------------------|---------------------|-------------------------|
| **Cluster Manager** | https://github.com/onedata/cluster-manager | Common Onedata component shared between Onezone and Oneprovider, which monitors and controls Onedata worker processes at site level. |
| **Cluster Worker** | https://github.com/onedata/cluster-worker | Common Onedata worker process implementation, shared between Onezone and Oneprovider. |

## Onezone

| Submodule | URL      | Description |
|-----------|----------|--------------|
| **Onezone worker** | https://github.com/onedata/oz-worker | Main Onezone functional component, based on the **Cluster Worker** framework. |

## Oneprovider

| Submodule | URL      | Description |
|-----------|----------|--------------|
| **Oneprovider worker** | https://github.com/onedata/op-worker | Main Oneprovider functional component, based on the **Cluster Worker** framework. |

## Oneclient

| Submodule | URL      | Description |
|-----------|----------|--------------|
| **Oneclient** | https://github.com/onedata/oneclient | Oneclient command line tool implementation. |

## Onepanel

| Submodule | URL      | Description |
|-----------|----------|--------------|
| **Onepanel** | https://github.com/onedata/onepanel | Onepanel administration service implementation. |

## LUMA

| Submodule | URL      | Description |
|-----------|----------|--------------|
| **LUMA** | https://github.com/onedata/luma | Local User MApping service reference implementation. |

## Other

| Submodule | URL      | Description |
|-----------|----------|--------------|
| **Appmock** | https://github.com/onedata/appmock |  Appmock is used during testing to mock any service which exposes REST API. |
| **Bamboo scripts** | https://github.com/onedata/bamboos | Bamboos is used for automating test deployments in [bamboo](https://www.atlassian.com/software/bamboo) during Onedata integration tests. |
| **Tests** | https://github.com/onedata/tests | Main Onedata tests repository. |

In order to initialize all submodules please use:
```bash
make submodules
```
instead of directly invoking Git `submodule` commands.

## Getting Started

The easiest way to get started with using or deploying Onedata is to start with our official [documentation](https://onedata.org/#/home/documentation/stable/index.html).

In order to try deploying Onedata, or specific components we have prepared a set of [example configurations and scenarios](https://github.com/onedata/getting-started).

## Building

The best way to use Onedata is to use our Docker images available at [Docker Hub](https://hub.docker.com/u/onedata/) or the binary packages available [here](https://get.onedata.org/). Currently the binary packages are only available for **Oneclient** component.

This repository can be also used to build entire Onedata system by invoking:

```bash
make
```

The build process itself is fully based on Docker containers, so no other prerequisites other than Docker should be necessary. In case of problems with Docker cache, please set `NO_CACHE=1` environment variable.

## Support

Please use [GitHub issues](https://github.com/onedata/onedata/issues) mechanism as the main channel for reporting bugs and requesting support or new features.

## Copyright and license

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

## Acknowledgements
This work was supported in part by 2017's research funds in the scope of the co-financed international projects framework (project no. 3711/H2020/2017/2).

This work is co-funded by the EOSC-hub project (Horizon 2020) under Grant number 777536.
