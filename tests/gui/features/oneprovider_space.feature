Feature: Oneprovider space functionality
  Various operations on spaces

  Background:
    Given user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "plgrid" login button
    And user of browser clicked on the "user1" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started
    And user of browser clicked on the "spaces" tab in main menu sidebar

  Scenario: User successfully creates new space with specified name (presses ENTER after entering space name)
    Given user of browser generates valid name string
    When user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types given name on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user of browser sees that a new item has appeared on spaces sidebar list

  Scenario: User successfully creates new space with specified name (clicks CREATE confirmation button after entering space name)
    Given user of browser generates valid name string
    When user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types given name on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees that a new item has appeared on spaces sidebar list

  # 'space1' defined in env.json
  Scenario: User successfully renames space (presses ENTER after entering space name)
    When user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for space named "space1"
    And user of browser sees that "Rename a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameSpace" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    And user of browser sees that the modal has disappeared
    Then user of browser sees that space1 has disappeared from spaces sidebar list
    And user of browser sees that NewNameSpace has appeared on spaces sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on settings icon displayed for "NewNameSpace" item on the spaces sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for space named "NewNameSpace"
    And user of browser sees that "Rename a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "space1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*NewNameSpace.*renamed.*space1.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that NewNameSpace has disappeared from spaces sidebar list
    And user of browser sees that space1 has appeared on spaces sidebar list

  # 'space1' defined in env.json
  Scenario: User successfully renames space (clicks OK confirmation button after entering space name)
    When user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for space named "space1"
    And user of browser sees that "Rename a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "NewNameSpace" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    And user of browser sees that the modal has disappeared
    Then user of browser sees that space1 has disappeared from spaces sidebar list
    And user of browser sees that NewNameSpace has appeared on spaces sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on settings icon displayed for "NewNameSpace" item on the spaces sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for space named "NewNameSpace"
    And user of browser sees that "Rename a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "space1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*NewNameSpace.*renamed.*space1.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that NewNameSpace has disappeared from spaces sidebar list
    And user of browser sees that space1 has appeared on spaces sidebar list

  # 'space1' defined in env.json
  Scenario: Check if "invite group" token box is not empty
    When user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "INVITE GROUP" item in settings dropdown for space named "space1"
    And user of browser sees that "Invite group to the space" modal has appeared
    Then user of browser sees non-empty token in active modal

  # 'space1' defined in env.json
  Scenario: Check if "get support" token box is not empty
    When user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "GET SUPPORT" item in settings dropdown for space named "space1"
    And user of browser sees that "Get support for the space" modal has appeared
    Then user of browser sees non-empty token in active modal

  # 'space1' defined in env.json
  Scenario: User fails to join to space because of using invalid token (presses ENTER after entering token)
    When user of browser clicks on the "Join" button in sidebar list's header
    And user of browser sees that "Join a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*[Ii]nvalid.*token.*
    And user of browser sees that the modal has disappeared

  # 'space1' defined in env.json
  Scenario: User fails to join to space because of using invalid token (clicks Join confirmation butto after entering token)
    When user of browser clicks on the "Join" button in sidebar list's header
    And user of browser sees that "Join a space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*[Ii]nvalid.*token.*
    And user of browser sees that the modal has disappeared

  # 'space2' defined in env.json
  Scenario: Switching between spaces
    When user of browser selects "space2" from spaces sidebar list
    And user of browser sees that submenu for space named "space2" has appeared
    And user of browser selects "space1" from spaces sidebar list
    Then user of browser sees that submenu for space named "space1" has appeared

  # 'space1' and 'space2' defined in env.json
  Scenario: Set given space as home and than set previous space as home
    When user of browser clicks on settings icon displayed for "space2" item on the spaces sidebar list
    And user of browser clicks on the "SET AS HOME" item in settings dropdown for space named "space2"
    Then user of browser sees an info notify with text matching to: .*space2.*home.*
    And user of browser sees that home space icon has appeared next to displayed name of space "space2" in spaces list

    # TODO rm code below after REST API become available
    And user of browser clicks on settings icon displayed for "space1" item on the spaces sidebar list
    And user of browser clicks on the "SET AS HOME" item in settings dropdown for space named "space1"
    And user of browser sees an info notify with text matching to: .*space1.*home.*
    And user of browser sees that home space icon has appeared next to displayed name of space "space1" in spaces list

  # 'space2' defined in env.json
  Scenario: Leave existing space and then create space with the same name
    When user of browser clicks on settings icon displayed for "C" item on the spaces sidebar list
    And user of browser clicks on the "LEAVE SPACE" item in settings dropdown for space named "C"
    And user of browser sees that "Leave a space" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    Then user of browser sees an info notify with text matching to: .*C.*left
    And user of browser sees that the modal has disappeared
    And user of browser sees that C has disappeared from spaces sidebar list

    # TODO rm code below after REST API become available
    And user of browser clicks on the "Create" button in sidebar list's header
    And user of browser sees that "Create a new space" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "C" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that C has appeared on spaces sidebar list
