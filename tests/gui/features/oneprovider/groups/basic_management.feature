Feature: Basic groups management in Oneprovider GUI


  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And initial groups configuration in "z1" Onezone service:
          group1:
            owner: user1
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


  Scenario: User receives group invitation token
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "INVITE GROUP" item in settings dropdown for group named "group1"
    And user of browser sees that "Invite group to the group" modal has appeared
    Then user of browser sees non-empty token in active modal


  Scenario: User fails to join group to space because of using invalid token (presses ENTER after entering token)
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "JOIN SPACE" item in settings dropdown for group named "group1"
    And user of browser sees that "Join a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*join.*group1.*space.*
    And user of browser sees that the modal has disappeared


  Scenario: User fails to join group to space because of using invalid token (clicks Join confirmation button after entering token)
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "JOIN SPACE" item in settings dropdown for group named "group1"
    And user of browser sees that "Join a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*join.*group1.*space.*
    And user of browser sees that the modal has disappeared


  Scenario: User fails to join group as subgroup because of using invalid token (presses ENTER after entering token)
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "JOIN AS SUBGROUP" item in settings dropdown for group named "group1"
    And user of browser sees that "Join a group to group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*join.*group1.*subgroup.*
    And user of browser sees that the modal has disappeared


  Scenario: User fails to join group as subgroup because of using invalid token (clicks Join confirmation button after entering token)
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "JOIN AS SUBGROUP" item in settings dropdown for group named "group1"
    And user of browser sees that "Join a group to group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*join.*group1.*subgroup.*
    And user of browser sees that the modal has disappeared


  Scenario: User fails to join to group because of using invalid token (presses ENTER after entering token)
    When user of browser clicks on the Join button in groups sidebar header
    And user of browser sees that "Join a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*[Ff]ailed.*join.*group.*
    And user of browser sees that the modal has disappeared


  Scenario: User fails to join to group because of using invalid token (clicks Join confirmation button after entering token)
    When user of browser clicks on the Join button in groups sidebar header
    And user of browser sees that "Join a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*[Ff]ailed.*join.*group.*
    And user of browser sees that the modal has disappeared


  Scenario: User successfully renames group (presses ENTER after entering group name)
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for group named "group1"
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameGroup" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*group1.*renamed.*NewNameGroup.*
    And user of browser sees that the modal has disappeared
    And user of browser refreshes site
    Then user of browser sees that group named "group1" has disappeared from the groups list
    And user of browser sees that group named "NewNameGroup" has appeared in the groups list


  Scenario: User successfully renames group (clicks OK after entering group name)
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for group named "group1"
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameGroup" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*group1.*renamed.*NewNameGroup.*
    And user of browser sees that the modal has disappeared
    And user of browser refreshes site
    Then user of browser sees that group named "group1" has disappeared from the groups list
    And user of browser sees that group named "NewNameGroup" has appeared in the groups list


  Scenario: User can leave existing group
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "LEAVE THIS GROUP" item in settings dropdown for group named "group1"
    And user of browser sees that "Leave the group" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees an info notify with text matching to: .*group1.*left
    And user of browser refreshes site
    And user of browser sees that group named "group1" has disappeared from the groups list


  Scenario: User fails to view group after leaving it
    When user of browser selects "group1" from groups sidebar list
    And user of browser copies a first resource ID from URL
    And user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "LEAVE THIS GROUP" item in settings dropdown for group named "group1"
    And user of browser sees that "Leave the group" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees an info notify with text matching to: .*group1.*left
    And user of browser refreshes site
    And user of browser sees that group named "group1" has disappeared from the groups list
    And user of browser changes webapp path to /#/onedata/groups concatenated with copied item
    Then user of browser sees an error notify with text matching to: .*?[Cc]annot load requested resource.*?
    And user of browser does not see "group1" in groups list
