Important notes
===============

- Currently the highest supported version of Firefox is 46.0.x - v47 is not supported due to incompatibility
with build-in selenium Firefox driver


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
./test_run.py -t tests/gui -i onedata/gui_builder:latest --test-type gui --driver=Firefox --self-contained-html --enable-logs
```

Used parameters:

* ``-t tests/gui`` - standard ``./test_run.py`` parameter to set the test cases path to gui tests
* ``-i onedata/gui_builder:latest`` - use Docker image with dependencied for GUI tests (i.a. Xvfb, Selenium, Firefox, Chrome)
* ``--test-type gui`` - set the test type use by core Onedata test helpers to differ from "cucumber" tests etc.
* ``--driver=<Firefox|Chrome>`` - set the browser to test in (will be launched in headless mode)
* ``--self-contained-html`` - optional, if used generated report will be contained in 1 html file
* ``--firefox-logs`` - optional, if used and driver is Firefox generated report will contain console logs from browser
* ``--xvfb`` - starts xvfb, necessary if used with headless tests
* ``--xvfb-recording=<all|none|failed>`` - optional, record all or none or failed tests as movies and save them to <logdir>/movies
* ``--no-mosaic-filter`` - optional, if set videos of tests using multiple browsers will be recorded as different video for each browser (mosaic video created by default) 

### Headless using existing Onedata installation

Using this method, the tests will be run using URL provided with ``--base-url=<url>`` parameter,
which should be url provided by starting environment for oz from env.json.
The URL should be a main application address of Onezone.

Example: (invoke from onedata repo root dir)
```
./test_run.py -t tests/gui -i onedata/gui_builder:latest --test-type gui --driver=Firefox --copy-etc-hosts --base-url=https://oz.1485165366.test --self-contained-html
```

New parameters:

* ``--copy-etc-hosts`` - optional, use if want to copy local contents of ``/etc/hosts`` file to docker, because some domains are defined locally

2. Non-headless using local machine (BDD)
-----------------------------------------------------

These tests will be run using ``py.test`` runner on local machine.
Required Python packages to install (e.g. using ``pip install``):

* pytest
* pytest_bdd
* git+git://github.com/bwalkowi/pytest-selenium-multi@orig-master
* git+https://github.com/bwalkowi/pytest-xvfb-recorder.git
* decorator

A browser selected for tests (with ``--driver``) should be also installed.

### Non-headless using existing Onedata installation

Example: (invoke from onedata repo root dir)
```
py.test --test-type=gui tests/gui --driver=Firefox --base-url=https://veilfsdev.com --self-contained-html
```


Test reports
============

The test report in HTML format with embedded screenshots of browser in failed test will be saved to:
``<onedata_repo_root>/tests/gui/logs/report.<time_stamp>/report.html``


Taking screenshots
==================

For some purposes, taking screenshots can be required in time of test run.

In steps of scenarios simply use:
```
driver.get_screenshot_as_file('/tmp/some-screenshot.png')
```
where driver is instance of Selenium WebDriver

Development
===========

Please read these section before you start writing or modifying GUI tests.

Fixtures and pytest plugins overrides
=====================================

* The default configuration of ``pytest-selenium-multi`` for sensitive URLs is inverted:
all tests are considered *non-destructive by default*.
You can add a ``@pytest.mark.destructive`` mark to test scenario to mark test as destructive.

* The ``sensitive_url`` fixture has module scope, because we start new environment for each module
(so it could have different ``base_url's``)
