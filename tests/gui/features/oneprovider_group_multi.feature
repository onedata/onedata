Feature: Oneprovider Group functionality using multiple browsers
  Various operations on groups using multiple browsers

  Background:
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened Onezone URL
    And users of [browser1, browser2] clicked on the "plgrid" login button
    And users of [browser1, browser2] logged as [user1, user3]
    And users of [browser1, browser2] expanded the "go to your files" Onezone sidebar panel
    And users of [browser1, browser2] clicked on the "p1" provider in Onezone providers sidebar panel
    And users of [browser1, browser2] clicked on the "Go to your files" button in provider popup
    And users of [browser1, browser2] clicked on the "groups" tab in main menu sidebar

  # TODO rm leave from group at the end of test
  Scenario: User successfully invites other user to join his group (clicks ENTER after entering token)
    # group 'group1' defined in env.json
    Given that in browser1 there is a "group1" item on the groups list
    When user of browser1 clicks a settings icon displayed for "group1" item on the groups list
    And user of browser1 sees a settings dropdown menu for "group1" item on the groups list
    And user of browser1 clicks on the "INVITE USER" item in current settings dropdown
    And user of browser1 sees that token box in "Invite user to the group" modal is active
    And user of browser1 sees non-empty token in active modal
    And user of browser1 clicks on copy button next to input box to copy visible token
    And user of browser1 sends copied token to users of [browser2]
    And user of browser1 clicks "OK" confirmation button in displayed modal
    And user of browser1 sees that "Invite user to the group" modal has disappeared
    And user of browser2 clicks on the "Join" button in groups sidebar
    And user of browser2 sees that input box in "Join a group" modal is active
    And user of browser2 types given token on keyboard
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that "Join a group" modal has disappeared
    Then user of browser2 sees that the "group1" has appeared on the groups list
    And user of browser2 selects "group1" from groups list
    And user of browser2 sees that "user3" item has appeared on current users permissions table
    And user of browser1 selects "group1" from groups list
    And user of browser1 sees that "user3" item has appeared on current users permissions table

    And user of browser2 clicks a settings icon displayed for "group1" item on the groups list
    And user of browser2 sees a settings dropdown menu for "group1" item on the groups list
    And user of browser2 clicks on the "LEAVE THIS GROUP" item in current settings dropdown
    And user of browser2 clicks "YES" confirmation button in displayed modal
    And user of browser2 sees that "Leave the group" modal has disappeared
    And user of browser2 sees an info notify with text matching to: .*group1.*left
    And user of browser2 sees that the "group1" has disappeared from the groups list

  # TODO rm leave from group at the end of test
  Scenario: User successfully invites other user to join his group (clicks JOIN confirmation button after entering token)
    # group 'group1' defined in env.json
    Given that in browser1 there is a "group1" item on the groups list
    When user of browser1 clicks a settings icon displayed for "group1" item on the groups list
    And user of browser1 sees a settings dropdown menu for "group1" item on the groups list
    And user of browser1 clicks on the "INVITE USER" item in current settings dropdown
    And user of browser1 sees that token box in "Invite user to the group" modal is active
    And user of browser1 sees non-empty token in active modal
    And user of browser1 clicks on copy button next to input box to copy visible token
    And user of browser1 sends copied token to users of [browser2]
    And user of browser1 clicks "OK" confirmation button in displayed modal
    And user of browser1 sees that "Invite user to the group" modal has disappeared
    And user of browser2 clicks on the "Join" button in groups sidebar
    And user of browser2 sees that input box in "Join a group" modal is active
    And user of browser2 types given token on keyboard
    And user of browser2 clicks "Join" confirmation button in displayed modal
    And user of browser2 sees that "Join a group" modal has disappeared
    Then user of browser2 sees that the "group1" has appeared on the groups list
    And user of browser2 selects "group1" from groups list
    And user of browser2 sees that "user3" item has appeared on current users permissions table
    And user of browser1 selects "group1" from groups list
    And user of browser1 sees that "user3" item has appeared on current users permissions table

    And user of browser2 clicks a settings icon displayed for "group1" item on the groups list
    And user of browser2 sees a settings dropdown menu for "group1" item on the groups list
    And user of browser2 clicks on the "LEAVE THIS GROUP" item in current settings dropdown
    And user of browser2 clicks "YES" confirmation button in displayed modal
    And user of browser2 sees that "Leave the group" modal has disappeared
    And user of browser2 sees an info notify with text matching to: .*group1.*left
    And user of browser2 sees that the "group1" has disappeared from the groups list
