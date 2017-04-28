Feature: Deployment process using panel of zone and provider


  Scenario: Cluster deployment
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [z1 zone panel, p1 provider panel] page
    And users of [browser1, browser2] entered credentials for [admin, admin] in login form
    And users of [browser1, browser2] pressed Sign in button

    When user of browser1 clicked on Create new cluster button in welcome page for "New cluster" sidebar item in oz panel
    And user of browser1 enables [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options for .*onezone.* host in hosts table in step 1 of deployment process for "New cluster" in oz panel
    And user of browser1 types "z1" to Zone name field in step 1 of deployment process for "New cluster" in oz panel
    And user of browser1 clicks on Deploy button in step 1 of deployment process for "New cluster" in oz panel
    And user of browser1 waits 60 seconds for cluster deployment to finish
    And user of browser1 clicks on Manage the cluster button in last step of deployment process for "z1" in oz panel

    And user of browser2 clicked on Create new cluster button in welcome page for "New cluster" sidebar item in oz panel
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

    When user of browser2 is idle for 5 seconds
