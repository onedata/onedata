Feature: Storage utilities using onepanel

  Background:
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [p1 provider panel, z1 onezone] page
    And user of browser1 entered credentials for admin in login form
    And users of browser1 pressed Sign in button
    And user of browser2 clicked on the "username" login button
    And user of browser2 seen that "Login with username and password" modal has appeared
    And user of browser2 entered credentials of admin in "Login with username and password" modal
    And user of browser2 clicked "Sign In" confirmation button in displayed modal


  Scenario: User uploads files on freshly supported space on newly created storage
    Given user of browser2 has 70 files in directory named "my_files"

    # create new_storage POSIX storage
    When user of browser1 clicks on Storages item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Add storage button in storages page in Onepanel
    And user of browser1 selects POSIX from storage selector in storages page in Onepanel
    And user of browser1 types "new_storage" to Storage name field in POSIX form in storages page in Onepanel
    And user of browser1 types "/mnt/st2" to Mount point field in POSIX form in storages page in Onepanel
    And user of browser1 clicks on Add button in add storage form in storages page in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Ss]torage.*added.*
    And user of browser1 expands "new_storage" record on storages list in storages page in Onepanel
    And user of browser1 sees that "new_storage" Storage type is posix in storages page in Onepanel
    And user of browser1 sees that "new_storage" Mount point is /mnt/st2 in storages page in Onepanel

    # create space
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
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
    And user of browser1 selects "new_storage" from storage selector in support space form in onepanel
    And user of browser1 types received token to Support token field in support space form in Onepanel
    And user of browser1 types "1" to Size input field in support space form in Onepanel
    And user of browser1 selects GB radio button in support space form in Onepanel
    And user of browser1 clicks on Support space button in support space form in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Aa]dded.*support.*space.*
    And user of browser1 sees that space support record for "helloworld" has appeared in Spaces page in Onepanel

    # confirm support of space
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "p1" provider in submenu of space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that provider popup for provider named "p1" has appeared on world map
    And user of browser2 clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser2 sees that Oneprovider session has started

    # create tmp dir and upload there 70 files
    And user of browser2 uses spaces select to change data space to "helloworld"
    And user of browser2 sees file browser in data tab in Oneprovider page
    And user of browser2 sees that current working directory displayed in breadcrumbs is helloworld
    And user of browser2 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser2 sees that "New directory" modal has appeared
    And user of browser2 clicks on input box in active modal
    And user of browser2 types "dir100" on keyboard
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that the modal has disappeared
    And user of browser2 sees that item named "dir100" has appeared in file browser
    And user of browser2 double clicks on item named "dir100" in file browser
    And user of browser2 sees that current working directory displayed in breadcrumbs is helloworld/dir100
    And user of browser2 uses upload button in toolbar to upload files from local directory "my_files" to remote current dir
    And user of browser2 waits for file upload to finish
    Then user of browser2 sees that there are 70 items in file browser

    # TODO rm after integrating with swagger
    # in order to change cwd to root dir change space to other than change back
    And user of browser2 changes current working directory to / using directory tree
    And user of browser2 sees that current working directory displayed in breadcrumbs is helloworld
    And user of browser2 selects "dir100" item(s) from file browser with pressed ctrl
    And user of browser2 clicks the button from top menu bar with tooltip "Remove element"
    And user of browser2 sees that "Remove files" modal has appeared
    And user of browser2 clicks "Yes" confirmation button in displayed modal
    And user of browser2 sees an info notify with text matching to: .*removed.*
    And user of browser2 sees that the modal has disappeared
    And user of browser2 sees that item named "dir100" has disappeared from files browser
