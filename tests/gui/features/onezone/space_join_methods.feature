Feature: Space join methods in Onezone GUI


  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
            - user2
    And initial spaces configuration in "z1" Onezone service:
        space1:
            owner: user2
            providers:
                - p1:
                    storage: onestorage
                    size: 1000000

    And users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [z1 onezone, z1 onezone] page
    And users of [browser1, browser2] logged as [user1, user2] to Onezone service
    And user of browser2 expanded the "go to your files" Onezone sidebar panel
    And user of browser2 clicked on "p1" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser2 clicked on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser2 seen that Oneprovider session has started
    And user of browser2 clicked on the "spaces" tab in main menu sidebar


  Scenario: User successfully joins space from Onezone gui level (presses ENTER after entering token)
    # invite user1 to space
    When user of browser2 clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser2 clicks on the "INVITE USER" item in settings dropdown for space named "space1"
    And user of browser2 sees that "Invite user to the space" modal has appeared
    And user of browser2 clicks on copy button in active modal
    And user of browser2 sees an info notify with text matching to: .*copied to clipboard.*
    And user of browser2 clicks "OK" confirmation button in displayed modal
    And user of browser2 sees that the modal has disappeared
    And user of browser2 sends copied token to user of browser1

    # user1 joins space
    And user of browser1 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser1 clicks on "Join space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser1 sees that "Join a space" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types received token on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared

    Then user of browser1 sees that space named "space1" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 selects "space1" from spaces sidebar list
    And user of browser2 refreshes site
    And user of browser2 sees that "user1" item has appeared on current USERS permissions table in Spaces tab


  Scenario: User successfully joins space from Onezone gui level (clicks JOIN confirmation button after entering token)
    # invite user1 to space
    When user of browser2 clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser2 clicks on the "INVITE USER" item in settings dropdown for space named "space1"
    And user of browser2 sees that "Invite user to the space" modal has appeared
    And user of browser2 clicks on copy button in active modal
    And user of browser2 sees an info notify with text matching to: .*copied to clipboard.*
    And user of browser2 clicks "OK" confirmation button in displayed modal
    And user of browser2 sees that the modal has disappeared
    And user of browser2 sends copied token to user of browser1

    # user1 joins space
    And user of browser1 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser1 clicks on "Join space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser1 sees that "Join a space" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types received token on keyboard
    And user of browser1 clicks "Join" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared

    Then user of browser1 sees that space named "space1" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 selects "space1" from spaces sidebar list
    And user of browser2 refreshes site
    And user of browser2 sees that "user1" item has appeared on current USERS permissions table in Spaces tab
