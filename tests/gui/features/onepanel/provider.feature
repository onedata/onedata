Feature: Provider details in onepanel


  Background:
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [p1 provider panel, z1 onezone] page
    And user of browser1 entered credentials for admin in login form
    And users of browser1 pressed Sign in button
    And user of browser2 seen Z1 zone name in login page
    And user of browser2 clicked on the "username" login button
    And user of browser2 seen that "Login with username and password" modal has appeared
    And user of browser2 entered credentials of admin in "Login with username and password" modal
    And user of browser2 clicked "Sign In" confirmation button in displayed modal


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


#  Scenario: User deregisters provider
#    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
#    And user of browser2 sees that there is space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser2 clicks on "p1" provider in submenu of space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser2 sees that provider popup for provider named "p1" has appeared on world map
#    And user of browser2 clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
#    And user of browser2 sees that Oneprovider session has started
##
##
##    And user of browser2 ...
#    And user of browser1 clicks on Provider item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
#    And user of browser1 clicks on Deregister provider button in provider page in Onepanel
#    And user of browser1 clicks on Yes, deregister button in Deregister provider popup
#    And user of browser1 sees an info notify with text matching to: .*[Pp]rovider.*deregistered.*
