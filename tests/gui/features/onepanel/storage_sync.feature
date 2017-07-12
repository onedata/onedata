Feature: Onepanel features regarding storage sync (e.g. import/update)

  Background:
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [p1 provider panel, z1 onezone] page
    And user of browser1 entered credentials for admin in login form
    And users of browser1 pressed Sign in button
    And user of browser2 seen Z1 zone name in oz login page
    And user of browser2 entered credentials of admin in login form in oz login page
    And user of browser2 clicked on the Sign in button in oz login page
    And directory tree structure on local file system:
          browser2:
              - dir1: 5
              - dir2:
                  - dir21:
                      - dir211:
                          - dir2111: 4
                      - file2.txt: 11111
                  - dir22: 10
                  - file1.txt: 22222


  Scenario: User supports space with storage sync and enabled options: Mount in root
    # create space
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is no space named "space2" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 types "space2" in active edit box
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that space named "space2" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # receive support token
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands settings dropdown for space named "space2" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "ADD STORAGE" item in settings dropdown for space named "space2" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that modal "Add storage" has appeared
    And user of browser2 sees non-empty token in "Add storage" modal
    And user of browser2 copies token from "Add storage" modal
    And user of browser2 sees an info notify with text matching to: .*copied.*
    And user of browser2 sends copied token to user of browser1

    # copy files to provider storage
    And user of browser2 copies dir2 to provider's storage mount point

    # support space
    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Support space button in spaces page in Onepanel if there are some spaces already supported
    And user of browser1 selects "onestorage" from storage selector in support space form in onepanel
    And user of browser1 types received token to Support token field in support space form in Onepanel
    And user of browser1 types "1" to Size input field in support space form in Onepanel
    And user of browser1 selects GB radio button in support space form in Onepanel

    # configure import parameters
    And user of browser1 enables Mount in root option in support space form in Onepanel
    And user of browser1 enables Import storage data option in support space form in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in IMPORT CONFIGURATION in support space form in Onepanel
    And user of browser1 types "2" to Max depth input field in IMPORT CONFIGURATION in support space form in Onepanel
    And user of browser1 clicks on Support space button in support space form in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Aa]dded.*support.*space.*
    And user of browser1 sees that space support record for "space2" has appeared in Spaces page in Onepanel

    # confirm correct import configuration
    And user of browser1 expands "space2" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Import strategy configuration for "space2" is as follow:
          Import strategy: Simple scan
          Max depth: 2

    # confirm support of space and go to provider
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "space2" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "p1" provider in submenu of space named "space2" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that provider popup for provider named "p1" has appeared on world map
    And user of browser2 clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser2 sees that Oneprovider session has started
    And user of browser2 uses spaces select to change data space to "space2"

    # confirm import of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    Then user of browser2 sees that there is 1 item in file browser
    And user of browser2 sees item(s) named "dir2" in file browser
    And user of browser2 double clicks on item named "dir2" in file browser

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space2/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space2/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "file1.txt" in file browser
    And user of browser2 sees that content of downloaded file "file1.txt" is equal to: "22222"

    # configure update parameters
    And user of browser1 expands toolbar for "space2" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in UPDATE CONFIGURATION in "space2" record in Spaces page in Onepanel
    And user of browser1 types "3" to Max depth input field in UPDATE CONFIGURATION in "space2" record in Spaces page in Onepanel
    And user of browser1 types "1" to Scan interval input field in UPDATE CONFIGURATION in "space2" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space2" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct update configuration
    And user of browser1 expands "space2" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Update strategy configuration for "space2" is as follow:
          Update strategy: Simple scan
          Max depth: 3
          Scan interval [s]: 1
          Write once: false
          Delete enabled: false

    # confirm update of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees that there are 2 items in file browser
    And user of browser2 sees item(s) named ["dir211", "file2.txt"] in file browser
    And user of browser2 double clicks on item named "file2.txt" in file browser
    And user of browser2 sees that content of downloaded file "file2.txt" is equal to: "11111"
    And user of browser2 double clicks on item named "dir211" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space2/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees that there are 10 items in file browser

    # revoke space support
    And user of browser1 expands toolbar for "space2" space record in Spaces page in Onepanel
    And user of browser1 clicks on Revoke space support option in space's toolbar in Onepanel
    And user of browser1 clicks on Yes, revoke button in REVOKE SPACE SUPPORT modal in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Ss]upport.*revoked.*


  Scenario: User supports space with storage sync and no enabled options

    # create space
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is no space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 types "space1" in active edit box
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that space named "space1" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # receive support token
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "ADD STORAGE" item in settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that modal "Add storage" has appeared
    And user of browser2 sees non-empty token in "Add storage" modal
    And user of browser2 copies token from "Add storage" modal
    And user of browser2 sees an info notify with text matching to: .*copied.*
    And user of browser2 sends copied token to user of browser1

    # support space
    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Support space button in spaces page in Onepanel if there are some spaces already supported
    And user of browser1 selects "onestorage" from storage selector in support space form in onepanel
    And user of browser1 types received token to Support token field in support space form in Onepanel
    And user of browser1 types "1" to Size input field in support space form in Onepanel
    And user of browser1 selects GB radio button in support space form in Onepanel
    And user of browser1 clicks on Support space button in support space form in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Aa]dded.*support.*space.*
    And user of browser1 sees that space support record for "space1" has appeared in Spaces page in Onepanel

    # copy files to provider storage
    And user of browser1 expands "space1" record on spaces list in Spaces page in Onepanel
    And user of browser1 copies Id of "space1" space in Spaces page in Onepanel
    And user of browser2 copies dir2 to the root directory of "space1" space

    # configure import parameters
    And user of browser1 expands toolbar for "space1" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in IMPORT CONFIGURATION in "space1" record in Spaces page in Onepanel
    And user of browser1 types "2" to Max depth input field in IMPORT CONFIGURATION in "space1" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space1" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct import configuration
    And user of browser1 expands "space1" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Import strategy configuration for "space1" is as follow:
          Import strategy: Simple scan
          Max depth: 2

    # confirm support of space and go to provider
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "p1" provider in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that provider popup for provider named "p1" has appeared on world map
    And user of browser2 clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser2 sees that Oneprovider session has started
    And user of browser2 uses spaces select to change data space to "space1"

    # confirm import of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    Then user of browser2 sees that there is 1 item in file browser
    And user of browser2 sees item(s) named "dir2" in file browser
    And user of browser2 double clicks on item named "dir2" in file browser

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space1/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space1/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "file1.txt" in file browser
    And user of browser2 sees that content of downloaded file "file1.txt" is equal to: "22222"

    # configure update parameters
    And user of browser1 expands toolbar for "space1" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in UPDATE CONFIGURATION in "space1" record in Spaces page in Onepanel
    And user of browser1 types "3" to Max depth input field in UPDATE CONFIGURATION in "space1" record in Spaces page in Onepanel
    And user of browser1 types "1" to Scan interval input field in UPDATE CONFIGURATION in "space1" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space1" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct update configuration
    And user of browser1 expands "space1" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Update strategy configuration for "space1" is as follow:
          Update strategy: Simple scan
          Max depth: 3
          Scan interval [s]: 1
          Write once: false
          Delete enabled: false

    # confirm update of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees that there are 2 items in file browser
    And user of browser2 sees item(s) named ["dir211", "file2.txt"] in file browser
    And user of browser2 double clicks on item named "file2.txt" in file browser
    And user of browser2 sees that content of downloaded file "file2.txt" is equal to: "11111"
    And user of browser2 double clicks on item named "dir211" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space1/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees that there are 10 items in file browser

    # revoke space support
    And user of browser1 expands toolbar for "space1" space record in Spaces page in Onepanel
    And user of browser1 clicks on Revoke space support option in space's toolbar in Onepanel
    And user of browser1 clicks on Yes, revoke button in REVOKE SPACE SUPPORT modal in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Ss]upport.*revoked.*


  Scenario: User supports space with storage sync and enabled options: Delete

    # create space
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is no space named "space3" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 types "space3" in active edit box
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that space named "space3" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # receive support token
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands settings dropdown for space named "space3" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "ADD STORAGE" item in settings dropdown for space named "space3" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that modal "Add storage" has appeared
    And user of browser2 sees non-empty token in "Add storage" modal
    And user of browser2 copies token from "Add storage" modal
    And user of browser2 sees an info notify with text matching to: .*copied.*
    And user of browser2 sends copied token to user of browser1

    # support space
    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Support space button in spaces page in Onepanel if there are some spaces already supported
    And user of browser1 selects "onestorage" from storage selector in support space form in onepanel
    And user of browser1 types received token to Support token field in support space form in Onepanel
    And user of browser1 types "1" to Size input field in support space form in Onepanel
    And user of browser1 selects GB radio button in support space form in Onepanel
    And user of browser1 clicks on Support space button in support space form in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Aa]dded.*support.*space.*
    And user of browser1 sees that space support record for "space3" has appeared in Spaces page in Onepanel

    # copy files to provider storage
    And user of browser1 expands "space3" record on spaces list in Spaces page in Onepanel
    And user of browser1 copies Id of "space3" space in Spaces page in Onepanel
    And user of browser2 copies dir2 to the root directory of "space3" space

    # configure import parameters
    And user of browser1 expands toolbar for "space3" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in IMPORT CONFIGURATION in "space3" record in Spaces page in Onepanel
    And user of browser1 types "2" to Max depth input field in IMPORT CONFIGURATION in "space3" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space3" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct import configuration
    And user of browser1 expands "space3" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Import strategy configuration for "space3" is as follow:
          Import strategy: Simple scan
          Max depth: 2

    # confirm support of space and go to provider
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "space3" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "p1" provider in submenu of space named "space3" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that provider popup for provider named "p1" has appeared on world map
    And user of browser2 clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser2 sees that Oneprovider session has started
    And user of browser2 uses spaces select to change data space to "space3"

    # confirm import of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    And user of browser2 sees that there is 1 item in file browser
    And user of browser2 sees item(s) named "dir2" in file browser
    And user of browser2 double clicks on item named "dir2" in file browser

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space3/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space3/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "file1.txt" in file browser
    And user of browser2 sees that content of downloaded file "file1.txt" is equal to: "22222"

    # configure update parameters
    And user of browser1 expands toolbar for "space3" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in UPDATE CONFIGURATION in "space3" record in Spaces page in Onepanel
    And user of browser1 types "3" to Max depth input field in UPDATE CONFIGURATION in "space3" record in Spaces page in Onepanel
    And user of browser1 types "1" to Scan interval input field in UPDATE CONFIGURATION in "space3" record in Spaces page in Onepanel
    And user of browser1 enables Delete enabled option UPDATE CONFIGURATION in "space3" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space3" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct update configuration
    And user of browser1 expands "space3" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Update strategy configuration for "space3" is as follow:
          Update strategy: Simple scan
          Max depth: 3
          Scan interval [s]: 1
          Write once: false
          Delete enabled: true

    # confirm update of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees that there are 2 items in file browser
    And user of browser2 sees item(s) named ["dir211", "file2.txt"] in file browser
    And user of browser2 double clicks on item named "file2.txt" in file browser
    And user of browser2 sees that content of downloaded file "file2.txt" is equal to: "11111"
    And user of browser2 double clicks on item named "dir211" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space3/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees that there are 10 items in file browser

    # confirm detection of deleted files
    And user of browser2 removes dir2/dir21 from the root directory of "space3" space
    And user of browser2 removes dir2/file1.txt from the root directory of "space3" space

    And user of browser2 changes current working directory to space3/dir2 using breadcrumbs
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    Then user of browser2 sees that there is 1 item in file browser
    And user of browser2 sees item(s) named "dir22" in file browser

    # revoke space support
    And user of browser1 expands toolbar for "space3" space record in Spaces page in Onepanel
    And user of browser1 clicks on Revoke space support option in space's toolbar in Onepanel
    And user of browser1 clicks on Yes, revoke button in REVOKE SPACE SUPPORT modal in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Ss]upport.*revoked.*


  Scenario: User supports space with storage sync and enabled options: Write once

    # create space
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is no space named "space4" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 types "space4" in active edit box
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that space named "space4" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # receive support token
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands settings dropdown for space named "space4" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "ADD STORAGE" item in settings dropdown for space named "space4" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that modal "Add storage" has appeared
    And user of browser2 sees non-empty token in "Add storage" modal
    And user of browser2 copies token from "Add storage" modal
    And user of browser2 sees an info notify with text matching to: .*copied.*
    And user of browser2 sends copied token to user of browser1

    # support space
    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Support space button in spaces page in Onepanel if there are some spaces already supported
    And user of browser1 selects "onestorage" from storage selector in support space form in onepanel
    And user of browser1 types received token to Support token field in support space form in Onepanel
    And user of browser1 types "1" to Size input field in support space form in Onepanel
    And user of browser1 selects GB radio button in support space form in Onepanel
    And user of browser1 clicks on Support space button in support space form in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Aa]dded.*support.*space.*
    And user of browser1 sees that space support record for "space4" has appeared in Spaces page in Onepanel

    # copy files to provider storage
    And user of browser1 expands "space4" record on spaces list in Spaces page in Onepanel
    And user of browser1 copies Id of "space4" space in Spaces page in Onepanel
    And user of browser2 copies dir2 to the root directory of "space4" space

    # configure import parameters
    And user of browser1 expands toolbar for "space4" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in IMPORT CONFIGURATION in "space4" record in Spaces page in Onepanel
    And user of browser1 types "2" to Max depth input field in IMPORT CONFIGURATION in "space4" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space4" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct import configuration
    And user of browser1 expands "space4" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Import strategy configuration for "space4" is as follow:
          Import strategy: Simple scan
          Max depth: 2

    # confirm support of space and go to provider
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "space4" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "p1" provider in submenu of space named "space4" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that provider popup for provider named "p1" has appeared on world map
    And user of browser2 clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser2 sees that Oneprovider session has started
    And user of browser2 uses spaces select to change data space to "space4"

    # confirm import of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    Then user of browser2 sees that there is 1 item in file browser
    And user of browser2 sees item(s) named "dir2" in file browser
    And user of browser2 double clicks on item named "dir2" in file browser

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space4/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space4/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "file1.txt" in file browser
    And user of browser2 sees that content of downloaded file "file1.txt" is equal to: "22222"

    # configure update parameters
    And user of browser1 expands toolbar for "space4" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in UPDATE CONFIGURATION in "space4" record in Spaces page in Onepanel
    And user of browser1 types "3" to Max depth input field in UPDATE CONFIGURATION in "space4" record in Spaces page in Onepanel
    And user of browser1 types "1" to Scan interval input field in UPDATE CONFIGURATION in "space4" record in Spaces page in Onepanel
    And user of browser1 enables Write once option UPDATE CONFIGURATION in "space4" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space4" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct update configuration
    And user of browser1 expands "space4" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Update strategy configuration for "space4" is as follow:
          Update strategy: Simple scan
          Max depth: 3
          Scan interval [s]: 1
          Write once: true
          Delete enabled: false

    # confirm update of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    Then user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees that there are 2 items in file browser
    And user of browser2 sees item(s) named ["dir211", "file2.txt"] in file browser
    And user of browser2 double clicks on item named "file2.txt" in file browser
    And user of browser2 sees that content of downloaded file "file2.txt" is equal to: "11111"
    And user of browser2 double clicks on item named "dir211" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space4/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees that there are 10 items in file browser

    # revoke space support
    And user of browser1 expands toolbar for "space4" space record in Spaces page in Onepanel
    And user of browser1 clicks on Revoke space support option in space's toolbar in Onepanel
    And user of browser1 clicks on Yes, revoke button in REVOKE SPACE SUPPORT modal in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Ss]upport.*revoked.*


  Scenario: User supports space with storage sync and enabled options: Deleta and Write once

    # create space
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is no space named "space5" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 types "space5" in active edit box
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that space named "space5" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # receive support token
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands settings dropdown for space named "space5" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "ADD STORAGE" item in settings dropdown for space named "space5" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that modal "Add storage" has appeared
    And user of browser2 sees non-empty token in "Add storage" modal
    And user of browser2 copies token from "Add storage" modal
    And user of browser2 sees an info notify with text matching to: .*copied.*
    And user of browser2 sends copied token to user of browser1

    # support space
    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Support space button in spaces page in Onepanel if there are some spaces already supported
    And user of browser1 selects "onestorage" from storage selector in support space form in onepanel
    And user of browser1 types received token to Support token field in support space form in Onepanel
    And user of browser1 types "1" to Size input field in support space form in Onepanel
    And user of browser1 selects GB radio button in support space form in Onepanel
    And user of browser1 clicks on Support space button in support space form in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Aa]dded.*support.*space.*
    And user of browser1 sees that space support record for "space5" has appeared in Spaces page in Onepanel

    # copy files to provider storage
    And user of browser1 expands "space5" record on spaces list in Spaces page in Onepanel
    And user of browser1 copies Id of "space5" space in Spaces page in Onepanel
    And user of browser2 copies dir2 to the root directory of "space5" space

    # configure import parameters
    And user of browser1 expands toolbar for "space5" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in IMPORT CONFIGURATION in "space5" record in Spaces page in Onepanel
    And user of browser1 types "2" to Max depth input field in IMPORT CONFIGURATION in "space5" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space5" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct import configuration
    And user of browser1 expands "space5" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Import strategy configuration for "space5" is as follow:
          Import strategy: Simple scan
          Max depth: 2

    # confirm support of space and go to provider
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "space5" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "p1" provider in submenu of space named "space5" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that provider popup for provider named "p1" has appeared on world map
    And user of browser2 clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser2 sees that Oneprovider session has started
    And user of browser2 uses spaces select to change data space to "space5"

    # confirm import of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    Then user of browser2 sees that there is 1 item in file browser
    And user of browser2 sees item(s) named "dir2" in file browser
    And user of browser2 double clicks on item named "dir2" in file browser

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space5/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space5/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "file1.txt" in file browser
    And user of browser2 sees that content of downloaded file "file1.txt" is equal to: "22222"

    # configure update parameters
    And user of browser1 expands toolbar for "space5" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in UPDATE CONFIGURATION in "space5" record in Spaces page in Onepanel
    And user of browser1 types "3" to Max depth input field in UPDATE CONFIGURATION in "space5" record in Spaces page in Onepanel
    And user of browser1 types "1" to Scan interval input field in UPDATE CONFIGURATION in "space5" record in Spaces page in Onepanel
    And user of browser1 enables Write once option UPDATE CONFIGURATION in "space5" record in Spaces page in Onepanel
    And user of browser1 enables Delete enabled option UPDATE CONFIGURATION in "space5" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space5" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct update configuration
    And user of browser1 expands "space5" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Update strategy configuration for "space5" is as follow:
          Update strategy: Simple scan
          Max depth: 3
          Scan interval [s]: 1
          Write once: true
          Delete enabled: false

    # confirm update of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees that there are 2 items in file browser
    And user of browser2 sees item(s) named ["dir211", "file2.txt"] in file browser
    And user of browser2 double clicks on item named "file2.txt" in file browser
    And user of browser2 sees that content of downloaded file "file2.txt" is equal to: "11111"
    And user of browser2 double clicks on item named "dir211" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space5/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees that there are 10 items in file browser

    # confirm detection of deleted files
    And user of browser2 removes dir2/dir21 from the root directory of "space5" space
    And user of browser2 removes dir2/file1.txt from the root directory of "space5" space

    And user of browser2 changes current working directory to space5/dir2 using breadcrumbs
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    And user of browser2 sees that there is 1 item in file browser
    And user of browser2 sees item(s) named "dir22" in file browser

    # revoke space support
    And user of browser1 expands toolbar for "space5" space record in Spaces page in Onepanel
    And user of browser1 clicks on Revoke space support option in space's toolbar in Onepanel
    And user of browser1 clicks on Yes, revoke button in REVOKE SPACE SUPPORT modal in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Ss]upport.*revoked.*


  Scenario: User disables files update

    # create space
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is no space named "space6" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 types "space6" in active edit box
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that space named "space6" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # receive support token
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands settings dropdown for space named "space6" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "ADD STORAGE" item in settings dropdown for space named "space6" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that modal "Add storage" has appeared
    And user of browser2 sees non-empty token in "Add storage" modal
    And user of browser2 copies token from "Add storage" modal
    And user of browser2 sees an info notify with text matching to: .*copied.*
    And user of browser2 sends copied token to user of browser1

    # support space
    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Support space button in spaces page in Onepanel if there are some spaces already supported
    And user of browser1 selects "onestorage" from storage selector in support space form in onepanel
    And user of browser1 types received token to Support token field in support space form in Onepanel
    And user of browser1 types "1" to Size input field in support space form in Onepanel
    And user of browser1 selects GB radio button in support space form in Onepanel
    And user of browser1 clicks on Support space button in support space form in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Aa]dded.*support.*space.*
    And user of browser1 sees that space support record for "space6" has appeared in Spaces page in Onepanel

    # copy files to provider storage
    And user of browser1 expands "space6" record on spaces list in Spaces page in Onepanel
    And user of browser1 copies Id of "space6" space in Spaces page in Onepanel
    And user of browser2 copies dir2 to the root directory of "space6" space

    # configure import parameters
    And user of browser1 expands toolbar for "space6" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in IMPORT CONFIGURATION in "space6" record in Spaces page in Onepanel
    And user of browser1 types "2" to Max depth input field in IMPORT CONFIGURATION in "space6" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space6" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct import configuration
    And user of browser1 expands "space6" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Import strategy configuration for "space6" is as follow:
          Import strategy: Simple scan
          Max depth: 2

    # confirm support of space and go to provider
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "space6" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "p1" provider in submenu of space named "space6" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that provider popup for provider named "p1" has appeared on world map
    And user of browser2 clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser2 sees that Oneprovider session has started
    And user of browser2 uses spaces select to change data space to "space6"

    # confirm import of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    And user of browser2 sees that there is 1 item in file browser
    And user of browser2 sees item(s) named "dir2" in file browser
    And user of browser2 double clicks on item named "dir2" in file browser

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space6/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space6/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "file1.txt" in file browser
    And user of browser2 sees that content of downloaded file "file1.txt" is equal to: "22222"

    # configure update parameters
    And user of browser1 expands toolbar for "space6" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in UPDATE CONFIGURATION in "space6" record in Spaces page in Onepanel
    And user of browser1 types "3" to Max depth input field in UPDATE CONFIGURATION in "space6" record in Spaces page in Onepanel
    And user of browser1 types "1" to Scan interval input field in UPDATE CONFIGURATION in "space6" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space6" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct update configuration
    And user of browser1 expands "space6" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Update strategy configuration for "space6" is as follow:
          Update strategy: Simple scan
          Max depth: 3
          Scan interval [s]: 1
          Write once: false
          Delete enabled: false

    # confirm update of files
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser

    And user of browser2 double clicks on item named "dir21" in file browser
    And user of browser2 sees that there are 2 items in file browser
    And user of browser2 sees item(s) named ["dir211", "file2.txt"] in file browser
    And user of browser2 double clicks on item named "file2.txt" in file browser
    And user of browser2 sees that content of downloaded file "file2.txt" is equal to: "11111"
    And user of browser2 double clicks on item named "dir211" in file browser
    And user of browser2 sees empty directory message in file browser
    And user of browser2 changes current working directory to space6/dir2 using breadcrumbs

    And user of browser2 double clicks on item named "dir22" in file browser
    And user of browser2 sees that there are 10 items in file browser

    # disable files update
    And user of browser1 expands toolbar for "space6" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Disabled strategy from strategy selector in UPDATE CONFIGURATION in "space6" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space6" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*support.*space.*changed.*

    # confirm correct update configuration
    And user of browser1 expands "space6" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Update strategy configuration for "space6" is as follow:
          Update strategy: Disabled

    # copy files to provider storage
    And user of browser2 copies dir1 to dir2 directory of "space6" space

    # confirm that new files were not detected
    And user of browser2 changes current working directory to space6/dir2 using breadcrumbs
    And user of browser2 is idle for 8 seconds
    And user of browser2 refreshes site
    And user of browser2 sees file browser in data tab in Oneprovider page

    And user of browser2 sees that there are 3 items in file browser
    And user of browser2 sees item(s) named ["dir21", "dir22", "file1.txt"] in file browser
    Then user of browser2 does not see any item(s) named "dir1" in file browser

    # revoke space support
    And user of browser1 expands toolbar for "space6" space record in Spaces page in Onepanel
    And user of browser1 clicks on Revoke space support option in space's toolbar in Onepanel
    And user of browser1 clicks on Yes, revoke button in REVOKE SPACE SUPPORT modal in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Ss]upport.*revoked.*
