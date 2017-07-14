Feature: Provider details in onepanel


  Background:
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [p1 provider panel, z1 onezone] page
    And user of browser1 entered credentials for admin in login form
    And users of browser1 pressed Sign in button
    And user of browser2 seen Z1 zone name in oz login page
    And user of browser2 entered credentials of admin in login form in oz login page
    And user of browser2 clicked on the Sign in button in oz login page


  Scenario: User changes provider name and redirection point
    When user of browser1 clicks on Provider item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 sees that Provider name attribute of provider has value of "p1" in Provider panel
    And user of browser1 sees that Redirection point provider attribute has ip value of "p1" provider in Provider panel

    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "p1" provider in submenu of space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that provider popup for provider named "p1" has appeared on world map
    And user of browser2 sees that hostname in displayed provider popup matches that of "p1" provider
    And user of browser2 clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser2 sees that Oneprovider session has started
    And user of browser2 sees that provider name displayed in Oneprovider page has value of "p1"

    # modify provider details
    And user of browser1 clicks on Provider item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Modify provider details button in provider page in Onepanel
    And user of browser1 types "pro1" to Provider name input box in modify provider details form in Provider panel
    And user of browser1 types "https://209.165.30.1" to Redirection point input box in modify provider details form in Provider panel
    And user of browser1 clicks on Modify provider details button in provider details form in Provider panel
    And user of browser1 sees an info notify with text matching to: .*[Pp]rovider.*data.*modified.*
    And user of browser1 sees that Provider name attribute of provider has value of "pro1" in Provider panel
    And user of browser1 sees that Redirection point attribute of provider has value of "https://209.165.30.1" in Provider panel

    # check if provider details were modified also in oz and op
    And user of browser2 is idle for 10 seconds
    And user of browser2 refreshes site
    And user of browser2 sees that provider name displayed in Oneprovider page has value of "pro1"
    And user of browser2 clicks on the "providers" tab in main menu sidebar
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "pro1" provider in submenu of space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that provider popup for provider named "pro1" has appeared on world map
    And user of browser2 sees that hostname in displayed provider popup matches 209.165.30.1 ip address

    # TODO rm after integration with swagger
    And user of browser1 clicks on Modify provider details button in provider page in Onepanel
    And user of browser1 types "p1" to Provider name input box in modify provider details form in Provider panel
    And user of browser1 types "p1" provider ip to Redirection point input box in modify provider details form in Provider panel
    And user of browser1 clicks on Modify provider details button in provider details form in Provider panel
    And user of browser1 sees an info notify with text matching to: .*[Pp]rovider.*data.*modified.*


  Scenario: User deregisters provider and registers it again
    # create space
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is no space named "tmp_space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 types "tmp_space" in active edit box
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that space named "tmp_space" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # receive support token
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands settings dropdown for space named "tmp_space" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "ADD STORAGE" item in settings dropdown for space named "tmp_space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that modal "Add storage" has appeared
    And user of browser2 sees non-empty token in "Add storage" modal
    And user of browser2 copies token from "Add storage" modal
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
    And user of browser1 sees that space support record for "tmp_space" has appeared in Spaces page in Onepanel

    # confirm support of space
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "tmp_space" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that list of supporting providers for space named "tmp_space" in expanded "DATA SPACE MANAGEMENT" Onezone panel contains only: "p1"

    # deregister provider
    And user of browser1 clicks on Provider item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Deregister provider button in provider page in Onepanel
    And user of browser1 clicks on Yes, deregister button in Deregister provider popup
    Then user of browser1 sees an info notify with text matching to: .*[Pp]rovider.*deregistered.*

    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "tmp_space" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that there is/are no supporting provider(s) named "p1" for space named "tmp_space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser2 sees that there is no provider in "GO TO YOUR FILES" Onezone panel

    # TODO when available check that after deregistration we are redirected to installation step 2

    # register it again
    And user of browser1 clicks on "New cluster" item in CLUSTERS sidebar in Onepanel
    And user of browser1 types "p1" to Provider name field in step 2 of deployment process in Onepanel
    And user of browser1 types ip address of "z1" zone to Onezone domain field in step 2 of deployment process in Onepanel
    And user of browser1 types redirection point of "p1" provider to Redirection point field in step 2 of deployment process in Onepanel
    And user of browser1 clicks on Register button in step 2 of deployment process in Onepanel
    And user of browser1 sees an info notify with text matching to: .*registered.*successfully.*

    # step3 in provider panel
    And user of browser1 expands "onestorage" record on storages list in step 3 of deployment process in Onepanel
    And user of browser1 sees that "onestorage" Storage type is posix in step 3 of deployment process in Onepanel
    And user of browser1 sees that "onestorage" Mount point is /volumes/storage in step 3 of deployment process in Onepanel

    And user of browser1 clicks on Finish button in step 3 of deployment process in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Ss]torage.*added.*
    And user of browser1 clicks on Manage the cluster button in last step of deployment process in Onepanel
    Then user of browser1 sees that [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options are enabled for .*oneprovider.* host in Nodes page in Onepanel
    And user of browser1 sees that [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options cannot be changed for .*oneprovider.* host in Nodes page in Onepanel

    # confirm that there is no supported space
    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 sees that list of supported spaces is empty in Spaces page in Onepanel

    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "tmp_space" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that there is/are no supporting provider(s) named "p1" for space named "tmp_space" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser2 sees that there is no provider in "GO TO YOUR FILES" Onezone panel
