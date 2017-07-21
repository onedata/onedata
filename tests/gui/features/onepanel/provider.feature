Feature: Provider management in Onepanel GUI


  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And initial spaces configuration in "z1" Onezone service:
        space1:
            owner: user1
            providers:
                - p1:
                    storage: onestorage
                    size: 1000000

    And users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [p1 provider panel, z1 onezone] page
    And user of browser1 logged as admin to Onepanel service
    And user of browser2 seen Z1 zone name in oz login page
    And user of browser2 logged as user1 to Onezone service


  Scenario: User changes provider name and redirection point
    When user of browser1 clicks on Provider item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 sees that Provider name attribute of provider has value of "p1" in Provider panel
    And user of browser1 sees that Redirection point provider attribute has ip value of "p1" provider in Provider panel

    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "p1" provider in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
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
    Then user of browser2 sees that provider name displayed in Oneprovider page has value of "pro1"
    And user of browser2 clicks on the "providers" tab in main menu sidebar
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "pro1" provider in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that provider popup for provider named "pro1" has appeared on world map
    And user of browser2 sees that hostname in displayed provider popup matches 209.165.30.1 ip address

    And user of browser1 clicks on Modify provider details button in provider page in Onepanel
    And user of browser1 types "p1" to Provider name input box in modify provider details form in Provider panel
    And user of browser1 types "p1" provider ip to Redirection point input box in modify provider details form in Provider panel
    And user of browser1 clicks on Modify provider details button in provider details form in Provider panel
    And user of browser1 sees an info notify with text matching to: .*[Pp]rovider.*data.*modified.*
    And user of browser1 is idle for 2 seconds


  Scenario: User deregisters provider and registers it again
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that list of supporting providers for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel contains only: "p1"

    # deregister provider
    And user of browser1 clicks on Provider item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Deregister provider button in provider page in Onepanel
    And user of browser1 clicks on Yes, deregister button in Deregister provider popup
    Then user of browser1 sees an info notify with text matching to: .*[Pp]rovider.*deregistered.*

    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that there is/are no supporting provider(s) named "p1" for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
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
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that there is/are no supporting provider(s) named "p1" for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser2 sees that there is no provider in "GO TO YOUR FILES" Onezone panel
