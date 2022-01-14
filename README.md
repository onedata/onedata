
![Onedata](resources/logo.png)

Onedata is a global data management system, providing easy
access to distributed storage resources, supporting wide range of use cases from
personal data management to data-intensive scientific computations.
Please visit the Onedata homepage (https://onedata.org) for more information, including the documentation and API specifications.

---

**Onedata** is composed of three components:

  * [Onezone](https://onedata.org/docs/doc/administering_onedata/onezone_overview.html) -
    allows connecting multiple storage providers (Oneprovider
    instances) into a distributed domain and offers a centralized Graphical User
    Interface for navigating the domain and performing data management tasks,
  * [Oneprovider](https://onedata.org/docs/doc/administering_onedata/provider_overview.html) -
    component deployed at each storage
    provider site, responsible for unifying and controlling access to data over
    low level storage resources of the provider,
  * [Oneclient](https://onedata.org/docs/doc/using_onedata/oneclient.html) -
    command line tool which enables transparent access to user data spaces
    through [Fuse](https://github.com/libfuse/libfuse) virtual filesystem,

Each of those components has its own code repository listed in the table below:


| Component | Repository      | 
|----------------------|---------------------|
| **Onezone** | https://github.com/onedata/onezone-pkg | 
| **Oneprovider** | https://github.com/onedata/oneprovider-pkg | 
| **Oneclient** | https://github.com/onedata/oneclient-pkg |


## Support

Please use [GitHub issues](https://github.com/onedata/onedata/issues) mechanism
as the main channel for reporting bugs and requesting support or new features.

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
This work was supported in part by 2017's research funds in the scope of the
co-financed international projects framework (project no. 3711/H2020/2017/2).

This work is co-funded by the EOSC-hub project (Horizon 2020) under Grant number 777536.
