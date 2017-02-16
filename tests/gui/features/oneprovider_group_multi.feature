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
    And users of [browser1, browser2] seen that Oneprovider session has started
    And users of [browser1, browser2] clicked on the "groups" tab in main menu sidebar


  # TODO rm leave from group at the end of test
  Scenario: User successfully invites other user to join his group (presses ENTER after entering token)
    # group 'group1' defined in env.json
    # TODO remove after loader will be added to sidebar list
    When user of browser1 refreshes site
    And user of browser1 clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser1 clicks on the "INVITE USER" item in settings dropdown for group named "group1"
    And user of browser1 sees that "Invite user to the group" modal has appeared
    And user of browser1 sees non-empty token in active modal
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sends copied token to user of browser2
    And user of browser1 clicks "OK" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared
    And user of browser2 clicks on the "Join" button in sidebar list's header
    And user of browser2 sees that "Join a group" modal has appeared
    And user of browser2 clicks on input box in active modal
    And user of browser2 types received token on keyboard
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that the modal has disappeared
    Then user of browser2 sees that group1 has appeared on groups sidebar list
    And user of browser2 selects "group1" from groups sidebar list
    And user of browser2 sees that "user3" item has appeared on current users permissions table
    And user of browser1 selects "group1" from groups sidebar list
    And user of browser1 sees that "user3" item has appeared on current users permissions table

    And user of browser2 clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser2 clicks on the "LEAVE THIS GROUP" item in settings dropdown for group named "group1"
    And user of browser2 sees that "Leave the group" modal has appeared
    And user of browser2 clicks "Yes" confirmation button in displayed modal
    And user of browser2 sees that the modal has disappeared
    And user of browser2 sees an info notify with text matching to: .*group1.*left
    And user of browser2 sees that group1 has disappeared from groups sidebar list


  # TODO rm leave from group at the end of test
  Scenario: User successfully invites other user to join his group (clicks JOIN confirmation button after entering token)
    # group 'group1' defined in env.json
    When user of browser1 clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser1 clicks on the "INVITE USER" item in settings dropdown for group named "group1"
    And user of browser1 sees that "Invite user to the group" modal has appeared
    And user of browser1 sees non-empty token in active modal
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sends copied token to user of browser2
    And user of browser1 clicks "OK" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared
    And user of browser2 clicks on the "Join" button in sidebar list's header
    And user of browser2 sees that "Join a group" modal has appeared
    And user of browser2 clicks on input box in active modal
    And user of browser2 types received token on keyboard
    And user of browser2 clicks "Join" confirmation button in displayed modal
    And user of browser2 sees that the modal has disappeared
    Then user of browser2 sees that group1 has appeared on groups sidebar list
    And user of browser2 selects "group1" from groups sidebar list
    And user of browser2 sees that "user3" item has appeared on current users permissions table
    And user of browser1 selects "group1" from groups sidebar list
    And user of browser1 sees that "user3" item has appeared on current users permissions table

    And user of browser2 clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser2 clicks on the "LEAVE THIS GROUP" item in settings dropdown for group named "group1"
    And user of browser2 sees that "Leave the group" modal has appeared
    And user of browser2 clicks "Yes" confirmation button in displayed modal
    And user of browser2 sees that the modal has disappeared
    And user of browser2 sees an info notify with text matching to: .*group1.*left
    And user of browser2 sees that group1 has disappeared from groups sidebar list


  Scenario: User fails to view group, to which he does not belong to, using its ID in URL
    # groups 'group1' defined in env.json
    When user of browser1 selects "group1" from groups sidebar list
    And user of browser1 copies a first resource ID from URL
    And user of browser1 sends copied group's ID to user of browser2
    And user of browser2 changes webapp path to /#/onedata/groups concatenated with received group's ID
    Then user of browser2 sees an error notify with text matching to: .*?[Cc]annot load requested resource.*?
    And user of browser2 does not see group1 in groups sidebar list
