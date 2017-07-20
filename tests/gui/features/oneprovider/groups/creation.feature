Feature: Groups creation in Oneprovider GUI


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

    And user opened browser window
    And user of browser opened z1 onezone page
    And user of browser logged as user1 to Onezone service
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on "p1" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicked on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser seen that Oneprovider session has started
    And user of browser clicked on the "groups" tab in main menu sidebar


  Scenario: User creates new group (presses ENTER after entering group name)
    When user of browser clicks on the Create button in groups sidebar header
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser refreshes site
    Then user of browser sees that group named "helloworld" has appeared in the groups list


  Scenario: User creates new group (clicks CREATE confirmation button after entering group name)
    When user of browser clicks on the Create button in groups sidebar header
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser refreshes site
    Then user of browser sees that group named "helloworld" has appeared in the groups list
