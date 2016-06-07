GUI acceptance/BDD tests
========================

GUI acceptance/BDD test can be run in few ways:
1. Using ``./test_run.py`` (from onedata repo root dir) - to start headless tests inside Docker on existing Onedata
   installation or start new Onedata environment - mainly for Continuous Integration.
2. Using ``py.test`` - to start non-headless tests on local machine - mainly for use Behaviour Driven Development
   or debugging. It also can be used to create screencasts.

If in doubt, simply use: ``make test_gui`` :)

1. Headless tests in Docker (CI)
--------------------------------

### Headless with automatic environment set up

Using this method, the Onedata environment will be set up automatically with OZ and OP (for details see ``environments``
dir with configurations). Setting up environment can take some time.

Example: (invoke from onedata repo root dir)
```
./test_run.py -t tests/gui -i onedata/gui_builder:selenium --test-type gui --driver=Firefox 
```

Used parameters:

* ``-t tests/gui`` - standard ``./test_run.py`` parameter to set the test cases path to gui tests
* ``-i onedata/gui_builder:selenium`` - use Docker image with dependencied for GUI tests (i.a. Xvfb, Selenium, Firefox, Chrome)
* ``--test-type gui`` - set the test type use by core Onedata test helpers to differ from "cucumber" tests etc.
* ``--driver=<Firefox|Chrome>`` - set the browser to test in (will be launched in headless mode)


### Headless using existing Onedata installation

Using this method, the tests will be run using URL provided with ``--base-url=<url>`` parameter.
The URL should be a main application address of Onezone.

Example: (invoke from onedata repo root dir)
```
./test_run.py -t tests/gui -i onedata/gui_builder:selenium --test-type gui --driver=Firefox --copy-etc-hosts --base-url=https://veilfsdev.com
```

New parameters:

* ``--copy-etc-hosts`` - optional, use if want to copy local contents of ``/etc/hosts`` file to docker, because some domains are defined locally

2. Non-headless using local machine (BDD)
-----------------------------------------------------

These tests will be run using ``py.test`` runner on local machine.
Required Python packages to install (e.g. using ``pip install``):

* pytest
* pytest_bdd
* pytest_selenium
* pytest_xvfb

A browser selected for tests (with ``--driver``) should be also installed.

Note, that ``--no-xvfb`` option is used to force to not use Xvfb even if it is installed in local system.

### Non-headless using existing Onedata installation

Example: (invoke from onedata repo root dir)
```
py.test tests/gui --driver=Firefox --no-xvfb --base-url=https://veilfsdev.com
```

<!-- TODO Below not used - probably to remove from readme -->

<!--### Non-headless with automatic environment set up-->

<!--Similar to above, but without specifying ``--base-url`` option.-->

<!--Note, that all requirements to start up Onedata in docker should be installed on local machine.-->

<!--Example: (invoke from onedata repo root dir)-->
<!--```-->
<!--py.test tests/gui --driver=Firefox --no-xvfb-->
<!--```-->


Test reports
============

The test report in HTML format with embedded screenshots of browser in failed test will be saved to:
``<onedata_repo_root>/tests/gui/logs/report.<time_stamp>/report.html``


Taking screenshots
==================

For some purposes, taking screenshots can be required in time of test run.

In steps of scenarios simply use:
```
selenium.get_screenshot_as_file('/tmp/some-screenshot.png')
```