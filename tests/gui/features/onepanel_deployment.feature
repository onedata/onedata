Feature: Deployment process using panel of zone and provider


  Scenario: Cluster deployment
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [z1 zone panel, p1 provider panel] page
    And users of [browser1, browser2] entered credentials for [admin, admin] in login form
    And users of [browser1, browser2] pressed Sign in button

    When user of browser1 clicks on Create new cluster button in welcome page for "New cluster" sidebar item in oz panel
    And user of browser1 enables [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options for .*onezone.* host in hosts table in step 1 of deployment process for "New cluster" in oz panel
    And user of browser1 types "z1" to Zone name field in step 1 of deployment process for "New cluster" in oz panel
    And user of browser1 clicks on Deploy button in step 1 of deployment process for "New cluster" in oz panel
    And user of browser1 waits 60 seconds for cluster deployment to finish
    And user of browser1 clicks on Manage the cluster button in last step of deployment process for "z1" in oz panel

    And user of browser2 clicks on Create new cluster button in welcome page for "New cluster" sidebar item in oz panel
    And user of browser2 enables [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options for .*oneprovider.* host in hosts table in step 1 of deployment process for "New cluster" in op panel
    And user of browser2 clicks on Deploy button in step 1 of deployment process for "New cluster" in op panel
    And user of browser2 waits 60 seconds for cluster deployment to finish

    And user of browser2 types "p1" to Provider name field in step 2 of deployment process for "New cluster" in op panel
    And user of browser2 types ip address of "z1" zone to Onezone domain field in step 2 of deployment process for "New cluster" in op panel
    And user of browser2 types redirection point of "p1" provider to Redirection point field in step 2 of deployment process for "New cluster" in op panel
    And user of browser2 clicks on Register button in step 2 of deployment process for "New cluster" in op panel
    And user of browser2 selects POSIX from storage selector in step 3 of deployment process for "p1" in op panel
    And user of browser2 types "onestorage" to Storage name field in add storage form in step 3 of deployment process for "p1" in op panel
    And user of browser2 types "/mnt/st1" to Mount point field in add storage form in step 3 of deployment process for "p1" in op panel
    And user of browser2 clicks on Add button in add storage form in step 3 of deployment process for "p1" in op panel

    And user of browser2 clicks on Finish button in step 3 of deployment process for "p1" in oz panel
    And user of browser2 clicks on Manage the cluster button in last step of deployment process for "p1" in op panel


  Scenario: Support space
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [p1 provider panel, z1 onezone] page
    And user of browser1 entered credentials for admin in login form
    And users of browser1 pressed Sign in button
    And user of browser2 clicked on the "username" login button
    And user of browser2 seen that "Login with username and password" modal has appeared
    And user of browser2 entered credentials of admin in "Login with username and password" modal
    And user of browser2 clicked "Sign In" confirmation button in displayed modal

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
    And user of browser1 clicks on Spaces item in submenu of "p1" item in op panel
    And user of browser1 clicks on Support space button in spaces for "p1" sidebar item in op panel
    And user of browser1 selects "onestorage" from storage selector in support space form for "p1" in op panel
    And user of browser1 types received token to Support token field in support space form for "p1" in op panel
    And user of browser1 types "1" to Size input field in support space form for "p1" in op panel
    And user of browser1 selects GB radio button in support space form for "p1" in op panel
    And user of browser1 clicks on Support space button in support space form for "p1" in op panel

    # confirm support of space
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that list of supporting providers for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel contains only: "p1"

    # TODO remove after integrate with swagger
    And user of browser2 expands settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "LEAVE" item in settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that "Leave a space" modal has appeared
    And user of browser2 clicks "Yes" confirmation button in displayed modal
    And user of browser2 sees that the modal has disappeared
    And user of browser2 sees that space named "helloworld" has disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel
