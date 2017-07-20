Feature: Basic spaces management in Oneprovider GUI


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
          space2:
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
    And user of browser clicked on the "spaces" tab in main menu sidebar


  Scenario: User successfully renames space (presses ENTER after entering space name)
    When user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for space named "space1"
    And user of browser sees that "Rename a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameSpace" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    And user of browser sees that the modal has disappeared
    And user of browser refreshes site
    Then user of browser sees that space named "space1" has disappeared from the spaces list
    And user of browser sees that space named "NewNameSpace" has appeared in the spaces list


  Scenario: User successfully renames space (clicks OK confirmation button after entering space name)
    When user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for space named "space1"
    And user of browser sees that "Rename a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameSpace" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    And user of browser sees that the modal has disappeared
    And user of browser refreshes site
    Then user of browser sees that space named "space1" has disappeared from the spaces list
    And user of browser sees that space named "NewNameSpace" has appeared in the spaces list


  Scenario: User fails to join to space because of using invalid token (presses ENTER after entering token)
    When user of browser clicks on the Join button in spaces sidebar header
    And user of browser sees that "Join a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*[Ii]nvalid.*token.*
    And user of browser sees that the modal has disappeared


  Scenario: User fails to join to space because of using invalid token (clicks Join confirmation button after entering token)
    When user of browser clicks on the Join button in spaces sidebar header
    And user of browser sees that "Join a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*[Ii]nvalid.*token.*
    And user of browser sees that the modal has disappeared


  Scenario: Switching between spaces
    When user of browser selects "space2" from spaces sidebar list
    And user of browser sees that space named "space2" is selected one in sidebar spaces list
    And user of browser selects "space1" from spaces sidebar list
    Then user of browser sees that space named "space1" is selected one in sidebar spaces list


  Scenario: Check if "invite group" token box is not empty
    When user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "INVITE GROUP" item in settings dropdown for space named "space1"
    And user of browser sees that "Invite group to the space" modal has appeared
    Then user of browser sees non-empty token in active modal


  Scenario: Check if "get support" token box is not empty
    When user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "GET SUPPORT" item in settings dropdown for space named "space1"
    And user of browser sees that "Get support for the space" modal has appeared
    Then user of browser sees non-empty token in active modal


  Scenario: Set given space as home
    When user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "SET AS HOME" item in settings dropdown for space named "space1"
    Then user of browser sees an info notify with text matching to: .*space1.*home.*
    And user of browser sees that space named "space1" is home space in spaces sidebar list


  Scenario: User leaves given space
    When user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "LEAVE SPACE" item in settings dropdown for space named "space1"
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*space1.*left
    And user of browser sees that the modal has disappeared
    And user of browser refreshes site
    Then user of browser sees that space named "space1" has disappeared from the spaces list
