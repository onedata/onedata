Feature: Import/update onepanel feature tests

  Background:
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [p1 provider panel, z1 onezone] page
    And user of browser1 entered credentials for admin in login form
    And users of browser1 pressed Sign in button
    And user of browser2 clicked on the "username" login button
    And user of browser2 seen that "Login with username and password" modal has appeared
    And user of browser2 entered credentials of admin in "Login with username and password" modal
    And user of browser2 clicked "Sign In" confirmation button in displayed modal
    And directory tree structure on local file system:
          browser2:
              - dir1: 5
              - dir2:
                  - dir21:
                      - dir211:
                          - dir2111: 4
                  - dir22: 10
                  - file1.txt: 1111111111


  Scenario: Import files

    # create space
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 types "helloworld" in active edit box
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that space named "helloworld" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 refreshes site

    # receive support token
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "GET SUPPORT" item in settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that dropright with token for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel has appeared
    And user of browser2 sees that dropright contains non-empty token for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 copy token from dropright for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees an info notify with text matching to: .*copied.*
    And user of browser2 sees that copied token matches displayed one
    And user of browser2 sends copied token to user of browser1

    # support space
    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Support space button in spaces page in Onepanel
    And user of browser1 selects "asd" from storage selector in support space form in onepanel
    And user of browser1 types received token to Support token field in support space form in Onepanel
    And user of browser1 types "1" to Size input field in support space form in Onepanel
    And user of browser1 selects GB radio button in support space form in Onepanel

    # configure import parameters
    And user of browser1 enables Import storage data option in support space form in Onepanel
    And user of browser1 selects Simple scan strategy from strategy selector in IMPORT CONFIGURATION in support space form in Onepanel
    And user of browser1 types "2" to Max depth input field in IMPORT CONFIGURATION in support space form in Onepanel
    And user of browser1 clicks on Support space button in support space form in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Aa]dded.*support.*space.*
    And user of browser1 sees that space support record for "helloworld" has appeared in Spaces page in Onepanel

    # assert correct parameters
    And user of browser1 expands "helloworld" record on spaces list in Spaces page in Onepanel
    And user of browser1 copies Id of "helloworld" space in Spaces page in Onepanel
    And user of browser1 sees that Import strategy configuration for "helloworld" is as follow:
          Import strategy: Simple scan
          Max depth: 2

    And user of browser2 copies dir2 to provider's storage mount point

    # confirm support of space
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "p1" provider in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that provider popup for provider named "p1" has appeared on world map
    And user of browser2 clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser2 sees that Oneprovider session has started
    And user of browser2 uses spaces select to change data space to "helloworld"
    And user of browser2 is idle for 20 seconds