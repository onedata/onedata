Feature: Onepanel features regarding storage sync (e.g. import/update)

  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1

    And users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [p1 provider panel, z1 onezone] page
    And user of browser1 logged as admin to Onepanel service
    And user of browser2 seen Z1 zone name in oz login page
    And user of browser2 logged as user1 to Onezone service
    And directory tree structure on local file system:
          browser2:
              - dir1: 500
              - dir2: 300


  Scenario: User configures storage sync and sees storage synchronization statistics

    # create space
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is no space named "space7" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 types "space7" to space creation edit box in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that space named "space7" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # receive support token
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands settings dropdown for space named "space7" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "ADD STORAGE" item in settings dropdown for space named "space7" in expanded "DATA SPACE MANAGEMENT" Onezone panel
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
    And user of browser1 sees that space support record for "space7" has appeared in Spaces page in Onepanel

    # copy files to provider storage
    And user of browser1 expands "space7" record on spaces list in Spaces page in Onepanel
    And user of browser1 copies Id of "space7" space in Spaces page in Onepanel
    And user of browser2 copies dir1 to dir0 directory of "space7" space

    # configure import parameters
    And user of browser1 expands toolbar for "space7" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in IMPORT CONFIGURATION in "space7" record in Spaces page in Onepanel
    And user of browser1 types "3" to Max depth input field in IMPORT CONFIGURATION in "space7" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space7" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*space.*support.*changed.*

    # confirm correct import configuration
    And user of browser1 expands "space7" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Import strategy configuration for "space7" is as follow:
          Import strategy: Simple scan
          Max depth: 3

    # check inserted number display on chart
    And user of browser1 is idle for 40 seconds
    Then user of browser1 sees that number of inserted files for "space7" shown on Synchronization files processing charts equals 502 in Spaces page in Onepanel

    # configure update parameters
    And user of browser1 expands toolbar for "space7" space record in Spaces page in Onepanel
    And user of browser1 clicks on Configure data synchronization option in space's toolbar in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in UPDATE CONFIGURATION in "space7" record in Spaces page in Onepanel
    And user of browser1 types "3" to Max depth input field in UPDATE CONFIGURATION in "space7" record in Spaces page in Onepanel
    And user of browser1 types "1" to Scan interval input field in UPDATE CONFIGURATION in "space7" record in Spaces page in Onepanel
    And user of browser1 enables Delete enabled option UPDATE CONFIGURATION in "space7" record in Spaces page in Onepanel
    And user of browser1 clicks on Save configuration button in "space7" record in Spaces page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Cc]onfiguration.*space.*support.*changed.*

    # confirm correct update configuration
    And user of browser1 expands "space7" record on spaces list in Spaces page in Onepanel
    And user of browser1 sees that Update strategy configuration for "space7" is as follow:
          Update strategy: Simple scan
          Max depth: 3
          Scan interval [s]: 1
          Write once: false
          Delete enabled: true

    # copy files to provider storage
    And user of browser2 copies dir2 to dir0 directory of "space7" space

    # check inserted and updated number display on chart
    And user of browser1 is idle for 40 seconds
    Then user of browser1 sees that number of inserted files for "space7" shown on Synchronization files processing charts equals 201 in Spaces page in Onepanel
    And user of browser1 sees that number of updated files for "space7" shown on Synchronization files processing charts equals 1 in Spaces page in Onepanel

    # confirm detection of deleted files
    And user of browser2 removes dir0 from the root directory of "space7" space
    And user of browser1 is idle for 30 seconds
    Then user of browser1 sees that number of deleted files for "space7" shown on Synchronization files processing charts equals 703 in Spaces page in Onepanel
    And user of browser1 sees that number of updated files for "space7" shown on Synchronization files processing charts equals 1 in Spaces page in Onepanel

    # revoke space support
    And user of browser1 expands toolbar for "space7" space record in Spaces page in Onepanel
    And user of browser1 clicks on Revoke space support option in space's toolbar in Onepanel
    And user of browser1 clicks on Yes, revoke button in REVOKE SPACE SUPPORT modal in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Ss]upport.*revoked.*
