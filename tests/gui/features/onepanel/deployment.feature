Feature: Deployment process using panel of zone and provider


  Scenario: Cluster deployment
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [z1 zone panel, p1 provider panel] page
    And users of [browser1, browser2] entered credentials for [admin, admin] in login form
    And users of [browser1, browser2] pressed Sign in button

    # step1 in zone and provider panels
    When user of browser1 clicks on Create new cluster button in welcome page in Onepanel
    And user of browser1 enables [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options for .*onezone.* host in step 1 of deployment process in Onepanel
    And user of browser1 types "z1" to Zone name field in step 1 of deployment process in Onepanel
    And user of browser1 clicks on Deploy button in step 1 of deployment process in Onepanel
    And user of browser1 sees that cluster deployment has started

    And user of browser2 clicks on Create new cluster button in welcome page in Onepanel
    And user of browser2 enables [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options for .*oneprovider.* host in step 1 of deployment process in Onepanel
    And user of browser2 clicks on Deploy button in step 1 of deployment process in Onepanel
    And user of browser2 sees that cluster deployment has started
    And user of browser2 waits 90 seconds for cluster deployment to finish
    And user of browser2 sees an info notify with text matching to: .*deployed.*successfully.*

    And user of browser1 waits 90 seconds for cluster deployment to finish
    And user of browser1 sees an info notify with text matching to: .*deployed.*successfully.*
    And user of browser1 clicks on Manage the cluster button in last step of deployment process in Onepanel
    Then user of browser1 sees that [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options are enabled for .*onezone.* host in Nodes page in Onepanel
    And user of browser1 sees that [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options cannot be changed for .*onezone.* host in Nodes page in Onepanel

    # step2 in provider panel
    And user of browser2 types "p1" to Provider name field in step 2 of deployment process in Onepanel
    And user of browser2 types ip address of "z1" zone to Onezone domain field in step 2 of deployment process in Onepanel
    And user of browser2 types redirection point of "p1" provider to Redirection point field in step 2 of deployment process in Onepanel
    And user of browser2 clicks on Register button in step 2 of deployment process in Onepanel
    And user of browser2 sees an info notify with text matching to: .*registered.*successfully.*

    # step3 in provider panel
    And user of browser2 selects POSIX from storage selector in step 3 of deployment process in Onepanel
    And user of browser2 types "onestorage" to Storage name field in POSIX form in step 3 of deployment process in Onepanel
    And user of browser2 types "/volumes/storage" to Mount point field in POSIX form in step 3 of deployment process in Onepanel
    And user of browser2 clicks on Add button in add storage form in step 3 of deployment process in Onepanel
    And user of browser2 sees an info notify with text matching to: .*[Ss]torage.*added.*
    And user of browser2 expands "onestorage" record on storages list in step 3 of deployment process in Onepanel
    And user of browser2 sees that "onestorage" Storage type is posix in step 3 of deployment process in Onepanel
#    And user of browser2 sees that "onestorage" Mount point is /volumes/storage in step 3 of deployment process in Onepanel

    And user of browser2 clicks on Finish button in step 3 of deployment process in Onepanel
    And user of browser2 sees an info notify with text matching to: .*[Ss]torage.*added.*
    And user of browser2 clicks on Manage the cluster button in last step of deployment process in Onepanel
    Then user of browser2 sees that [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options are enabled for .*oneprovider.* host in Nodes page in Onepanel
    And user of browser2 sees that [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options cannot be changed for .*oneprovider.* host in Nodes page in Onepanel
