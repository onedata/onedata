Feature: Oneprovider Group functionality
  Various operations on groups

  # user 'user1' defined in env.json
  # provider 'p1' defined in env.json
  Background:
    Given user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "plgrid" login button
    And user of browser clicked on the "user1" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started
    And user of browser clicked on the "groups" tab in main menu sidebar

  Scenario: User creates new group (presses ENTER after entering group name)
    Given user of browser generates valid name string
    When user of browser clicks on the "Create" button in groups sidebar
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types given name on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that modal has disappeared
    Then user of browser sees that the new item has appeared on the groups list

  Scenario: User creates new group (clicks CREATE confirmation button after entering group name)
    Given user of browser generates valid name string
    When user of browser clicks on the "Create" button in groups sidebar
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types given name on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that modal has disappeared
    Then user of browser sees that the new item has appeared on the groups list

  Scenario: User can invite group
    # group 'group1' defined in env.json
    Given that in browser there is a "group1" item on the groups list
    When user of browser clicks a settings icon displayed for "group1" item on the groups list
    And user of browser sees a settings dropdown menu for "group1" item on the groups list
    And user of browser clicks on the "INVITE GROUP" item in current settings dropdown
    And user of browser sees that "Invite group to the group" modal has appeared
    Then user of browser sees non-empty token in active modal

  Scenario: User can request space creation
    # group 'group1' defined in env.json
    Given that in browser there is a "group1" item on the groups list
    When user of browser clicks a settings icon displayed for "group1" item on the groups list
    And user of browser sees a settings dropdown menu for "group1" item on the groups list
    And user of browser clicks on the "REQUEST SPACE CREATION" item in current settings dropdown
    And user of browser sees that "Request space creation for the group" modal has appeared
    Then user of browser sees non-empty token in active modal

  Scenario: User fails to join group to space because of using invalid token (presses ENTER after entering token)
    # group 'group1' defined in env.json
    Given that in browser there is a "group1" item on the groups list
    When user of browser clicks a settings icon displayed for "group1" item on the groups list
    And user of browser sees a settings dropdown menu for "group1" item on the groups list
    And user of browser clicks on the "JOIN SPACE" item in current settings dropdown
    And user of browser sees that "Join a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*join.*group1.*space.*

  Scenario: User fails to join group to space because of using invalid token (clicks Join confirmation button after entering token)
    # group 'group1' defined in env.json
    Given that in browser there is a "group1" item on the groups list
    When user of browser clicks a settings icon displayed for "group1" item on the groups list
    And user of browser sees a settings dropdown menu for "group1" item on the groups list
    And user of browser clicks on the "JOIN SPACE" item in current settings dropdown
    And user of browser sees that "Join a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*join.*group1.*space.*

  Scenario: User fails to join group as subgroup because of using invalid token (presses ENTER after entering token)
    # group 'group1' defined in env.json
    Given that in browser there is a "group1" item on the groups list
    When user of browser clicks a settings icon displayed for "group1" item on the groups list
    And user of browser sees a settings dropdown menu for "group1" item on the groups list
    And user of browser clicks on the "JOIN AS SUBGROUP" item in current settings dropdown
    And user of browser sees that "Join a group to group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*join.*group1.*subgroup.*

  Scenario: User fails to join group as subgroup because of using invalid token (clicks Join confirmation button after entering token)
    # group 'group1' defined in env.json
    Given that in browser there is a "group1" item on the groups list
    When user of browser clicks a settings icon displayed for "group1" item on the groups list
    And user of browser sees a settings dropdown menu for "group1" item on the groups list
    And user of browser clicks on the "JOIN AS SUBGROUP" item in current settings dropdown
    And user of browser sees that "Join a group to group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*join.*group1.*subgroup.*

  Scenario: User fails to join to group because of using invalid token (presses ENTER after entering token)
    When user of browser clicks on the "Join" button in groups sidebar
    And user of browser sees that "Join a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*[Ff]ailed.*join.*group.*

  Scenario: User fails to join to group because of using invalid token (clicks Join confirmation button after entering token)
    When user of browser clicks on the "Join" button in groups sidebar
    And user of browser sees that "Join a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*[Ff]ailed.*join.*group.*

  Scenario: User successfully renames group (presses ENTER after entering group name)
    # group 'group1' defined in env.json
    Given that in browser there is a "group1" item on the groups list
    When user of browser clicks a settings icon displayed for "group1" item on the groups list
    And user of browser sees a settings dropdown menu for "group1" item on the groups list
    And user of browser clicks on the "RENAME" item in current settings dropdown
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameGroup" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*group1.*renamed.*NewNameGroup.*
    And user of browser sees that modal has disappeared
    Then user of browser sees that the "group1" has disappeared from the groups list
    And user of browser sees that the "NewNameGroup" has appeared on the groups list
    # TODO rm code below after REST API become available
    And user of browser clicks a settings icon displayed for "NewNameGroup" item on the groups list
    And user of browser sees a settings dropdown menu for "NewNameGroup" item on the groups list
    And user of browser clicks on the "RENAME" item in current settings dropdown
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "group1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*NewNameGroup.*renamed.*group1.*
    And user of browser sees that modal has disappeared
    And user of browser sees that the "NewNameGroup" has disappeared from the groups list
    And user of browser sees that the "group1" has appeared on the groups list

  Scenario: User successfully renames group (clicks OK after entering group name)
    # group 'group1' defined in env.json
    Given that in browser there is a "group1" item on the groups list
    When user of browser clicks a settings icon displayed for "group1" item on the groups list
    And user of browser sees a settings dropdown menu for "group1" item on the groups list
    And user of browser clicks on the "RENAME" item in current settings dropdown
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameGroup" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*group1.*renamed.*NewNameGroup.*
    And user of browser sees that modal has disappeared
    Then user of browser sees that the "group1" has disappeared from the groups list
    And user of browser sees that the "NewNameGroup" has appeared on the groups list
    # TODO rm code below after REST API become available
    And user of browser clicks a settings icon displayed for "NewNameGroup" item on the groups list
    And user of browser sees a settings dropdown menu for "NewNameGroup" item on the groups list
    And user of browser clicks on the "RENAME" item in current settings dropdown
    And user of browser sees that "Rename a group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "group1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*NewNameGroup.*renamed.*group1.*
    And user of browser sees that modal has disappeared
    And user of browser sees that the "NewNameGroup" has disappeared from the groups list
    And user of browser sees that the "group1" has appeared on the groups list

  Scenario: User can leave existing group and then create group with the same name
    # group 'group1' defined in env.json
    Given that in browser there is a "group1" item on the groups list
    When user of browser clicks a settings icon displayed for "group1" item on the groups list
    And user of browser sees a settings dropdown menu for "group1" item on the groups list
    And user of browser clicks on the "LEAVE THIS GROUP" item in current settings dropdown
    And user of browser sees that "Leave the group" modal has appeared
    And user of browser clicks "YES" confirmation button in displayed modal
    And user of browser sees that modal has disappeared
    Then user of browser sees an info notify with text matching to: .*group1.*left
    And user of browser sees that the "group1" has disappeared from the groups list
    # TODO rm code below after REST API become available
    And user of browser clicks on the "Create" button in groups sidebar
    And user of browser sees that "Create a new group" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "group1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that modal has disappeared
    And user of browser sees that the "group1" has appeared on the groups list
