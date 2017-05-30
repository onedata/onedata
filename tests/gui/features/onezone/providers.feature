Feature: Onezone GUI elements
  A user interface for managing Onezone account


  Background:
    Given user opened browser window

    And user of browser has account in "z1" Onezone service
    And initial spaces configuration in "z1" Onezone service:
        space1:
            owner: browser
            providers:
                - p1:
                    storage: asd
                    size: 1000000

    And user of browser opened z1 onezone page
    And user of browser clicked on the "username" login button
    And user of browser seen that "Login with username and password" modal has appeared
    And user of browser entered his credentials in "Login with username and password" modal
    And user of browser clicked "Sign In" confirmation button in displayed modal


  Scenario: User opens provider popup by clicking on supporting provider in space's submenu
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "p1" provider in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that provider popup for provider named "p1" has appeared on world map


  Scenario: User can go to Oneprovider by clicking on Go to your files in provider's popup
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "p1" provider in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that provider popup for provider named "p1" has appeared on world map
    And user of browser clicks on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser sees that Oneprovider session has started
    Then user of browser sees that URL matches: https?://[^/]*/#/onedata/data/.*


  Scenario: User sees that after unsupporting space, number displayed in space counter for given provider decreases
    When user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser sees that there is provider named "p1" in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser sees that spaces counter for provider named "p1" displays 1 in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser expands submenu of provider named "p1" by clicking on cloud in provider record in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser sees that spaces counter for "p1" match number of displayed supported spaces in expanded submenu of given provider in expanded "GO TO YOUR FILES" Onezone panel

    And user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser expands settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser clicks on the "LEAVE" item in settings dropdown for space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that space named "space1" has disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel

    Then user of browser sees that spaces counter for provider named "p1" displays 0 in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser expands submenu of provider named "p1" by clicking on cloud in provider record in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser sees that spaces counter for "p1" match number of displayed supported spaces in expanded submenu of given provider in expanded "GO TO YOUR FILES" Onezone panel


  Scenario: User can set Provider as Home provider (icon changes), and when he relogins, he will be redirected to Home provider automatically
    When user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser sees that there is provider named "p1" in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser sets provider named "p1" as home by clicking on home outline in that provider record in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser sees that provider named "p1" is set as home provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser expands account settings dropdown in "ACCOUNT MANAGE" Onezone top bar
    And user of browser clicks on LOGOUT item in expanded settings dropdown in "ACCOUNT MANAGE" Onezone top bar
    And user of browser should see that the page title contains "Login"

    And user of browser clicks on the "username" login button
    And user of browser sees that "Login with username and password" modal has appeared
    And user of browser enters his credentials in "Login with username and password" modal
    And user of browser clicks "Sign In" confirmation button in displayed modal
    Then user of browser sees that Oneprovider session has started


  Scenario: User sees that after clicking on provider's circle on world map, provider's popup appears
    When user of browser sees that there is no displayed provider popup next to 1st provider circle on Onezone world map
    And user of browser clicks on 1st provider circle on Onezone world map
    Then user of browser sees that provider popup has appeared next to 1st provider circle on Onezone world map


  # TODO clicking on other provider do when there will be other providers
  Scenario: User sees that provider popup can be closed with clicking the same or other provider circle
    When user of browser sees that there is no displayed provider popup next to 1st provider circle on Onezone world map
    And user of browser clicks on 1st provider circle on Onezone world map
    And user of browser sees that provider popup has appeared next to 1st provider circle on Onezone world map
    And user of browser clicks on 1st provider circle on Onezone world map
    Then user of browser sees that provider popup next to 1st provider circle on Onezone world map has disappeared


  # TODO now it is not possible to close by clicking outside map, try it when it will be possible
  Scenario: User sees that provider popup can be closed with clicking on map
    When user of browser sees that there is no displayed provider popup next to 1st provider circle on Onezone world map
    And user of browser clicks on 1st provider circle on Onezone world map
    And user of browser sees that provider popup has appeared next to 1st provider circle on Onezone world map
    And user of browser clicks on Onezone world map
    Then user of browser sees that provider popup next to 1st provider circle on Onezone world map has disappeared


  Scenario: User sees that spaces list in provider popup and provider record in "GO TO YOUR FILES" matches
    When user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser expands submenu of provider named "p1" by clicking on cloud in provider record in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicks on "p1" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser sees that provider popup for provider named "p1" has appeared on world map
    Then user of browser sees that the list of spaces in provider popup and in expanded "GO TO YOUR FILES" Onezone panel are the same for provider named "p1"


  Scenario: User sees that when no provider is working appropriate msg is shown
    Given there are no working provider(s) named "p1"
    When user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    Then user of browser sees that provider named "p1" in expanded "GO TO YOUR FILES" Onezone panel is not working
    And user of browser sees alert with title "All your providers are offline" on world map in Onezone gui


  Scenario: User sees that if space is displayed in provider submenu in GO TO YOUR FILES panel, that provider is also displayed in submenu of that space in DATA SPACE MANAGEMENT panel
    When user of browser expands the "GO TO YOUR FILES" Onezone sidebar panel
    And user of browser sees that there is provider named "p1" in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser expands submenu of provider named "p1" by clicking on cloud in provider record in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser sees that there is space named "space1" in submenu of provider named "p1" in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser expands submenu of space named "space1" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that there is provider named "p1" in submenu of space named "space1" in expanded "DATA SPACE MANAGEMENT" Onezone panel
