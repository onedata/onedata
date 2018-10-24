Feature: Oneprovider Group functionality
  Various operations on groups

  # user 'user1' defined in env.json
  # provider 'p1' defined in env.json
  Background:
    Given user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "devLogin" login button
    And user of browser clicked on the "user1" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started
    And user of browser clicked on the "groups" tab in main menu sidebar


  Scenario: User creates new group (presses ENTER after entering group name)
    Given user of browser generates valid name string
    When user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types given name on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user of browser sees that a new item has appeared on groups sidebar list


  Scenario: User creates new group (clicks CREATE confirmation button after entering group name)
    Given user of browser generates valid name string
    When user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types given name on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees that a new item has appeared on groups sidebar list


  Scenario: User can invite group
    # group 'group1' defined in env.json
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "INVITE GROUP" item in settings dropdown for group named "group1"
    And user of browser sees that "Invite group to the group" modal has appeared
    Then user of browser sees non-empty token in active modal


  Scenario: User fails to join group to space because of using invalid token (presses ENTER after entering token)
    # group 'group1' defined in env.json
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "JOIN SPACE" item in settings dropdown for group named "group1"
    And user of browser sees that "Join a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*join.*group1.*space.*
    And user of browser sees that the modal has disappeared


  Scenario: User fails to join group to space because of using invalid token (clicks Join confirmation button after entering token)
    # group 'group1' defined in env.json
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "JOIN SPACE" item in settings dropdown for group named "group1"
    And user of browser sees that "Join a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*join.*group1.*space.*
    And user of browser sees that the modal has disappeared


  Scenario: User fails to join group as subgroup because of using invalid token (presses ENTER after entering token)
    # group 'group1' defined in env.json
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "JOIN AS SUBGROUP" item in settings dropdown for group named "group1"
    And user of browser sees that "Join a group to group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*join.*group1.*subgroup.*
    And user of browser sees that the modal has disappeared


  Scenario: User fails to join group as subgroup because of using invalid token (clicks Join confirmation button after entering token)
    # group 'group1' defined in env.json
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "JOIN AS SUBGROUP" item in settings dropdown for group named "group1"
    And user of browser sees that "Join a group to group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*join.*group1.*subgroup.*
    And user of browser sees that the modal has disappeared


  Scenario: User fails to join to group because of using invalid token (presses ENTER after entering token)
    When user of browser clicks on the "Join" button in sidebar list's header
    And user of browser sees that "Join a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*[Ff]ailed.*join.*group.*
    And user of browser sees that the modal has disappeared


  Scenario: User fails to join to group because of using invalid token (clicks Join confirmation button after entering token)
    When user of browser clicks on the "Join" button in sidebar list's header
    And user of browser sees that "Join a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*[Ff]ailed.*join.*group.*
    And user of browser sees that the modal has disappeared


  Scenario: User successfully renames group (presses ENTER after entering group name)
    # group 'group1' defined in env.json
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for group named "group1"
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameGroup" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*group1.*renamed.*NewNameGroup.*
    And user of browser sees that the modal has disappeared
    Then user of browser sees that group1 has disappeared from groups sidebar list
    And user of browser sees that NewNameGroup has appeared on groups sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on settings icon displayed for "NewNameGroup" item on the groups sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for group named "NewNameGroup"
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "group1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*NewNameGroup.*renamed.*group1.*
    And user of browser sees that the modal has disappeared
    Then user of browser sees that NewNameGroup has disappeared from groups sidebar list
    And user of browser sees that group1 has appeared on groups sidebar list


  Scenario: User successfully renames group (clicks OK after entering group name)
    # group 'group1' defined in env.json
    When user of browser clicks on settings icon displayed for "group1" item on the groups sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for group named "group1"
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameGroup" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*group1.*renamed.*NewNameGroup.*
    And user of browser sees that the modal has disappeared
    Then user of browser sees that group1 has disappeared from groups sidebar list
    And user of browser sees that NewNameGroup has appeared on groups sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on settings icon displayed for "NewNameGroup" item on the groups sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for group named "NewNameGroup"
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "group1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*NewNameGroup.*renamed.*group1.*
    And user of browser sees that the modal has disappeared
    Then user of browser sees that NewNameGroup has disappeared from groups sidebar list
    And user of browser sees that group1 has appeared on groups sidebar list


  Scenario: User can leave existing group and then create group with the same name
    # group 'group1' defined in env.json
    When user of browser clicks on settings icon displayed for "group3" item on the groups sidebar list
    And user of browser clicks on the "LEAVE THIS GROUP" item in settings dropdown for group named "group3"
    And user of browser sees that "Leave the group" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees an info notify with text matching to: .*group3.*left
    And user of browser is idle for 4 seconds
    And user of browser sees that group3 has disappeared from groups sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "group3" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that group3 has appeared on groups sidebar list


  Scenario: User fails to view group after leaving it
    # TODO rm after integration with swagger
    When user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "tmp_group" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    And user of browser selects "tmp_group" from groups sidebar list
    And user of browser copies a first resource ID from URL
    And user of browser clicks on settings icon displayed for "tmp_group" item on the groups sidebar list
    And user of browser clicks on the "LEAVE THIS GROUP" item in settings dropdown for group named "tmp_group"
    And user of browser sees that "Leave the group" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees an info notify with text matching to: .*tmp_group.*left
    And user of browser is idle for 4 seconds
    And user of browser sees that tmp_group has disappeared from groups sidebar list
    And user of browser changes webapp path to /#/onedata/groups concatenated with copied item
    Then user of browser sees an error notify with text matching to: .*?[Cc]annot load requested resource.*?
    And user of browser does not see tmp_group in groups sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "tmp_group" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that tmp_group has appeared on groups sidebar list
