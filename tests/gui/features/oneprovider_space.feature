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
    And user of browser clicked on the "spaces" tab in main menu sidebar

  Scenario: User successfully creates new space with specified name (clicks ENTER after entering space name)
    Given user of browser generates valid name string
    When user of browser clicks on the "Create" button in spaces sidebar
    And user of browser sees that input box in "Create a new space" modal is active
    And user of browser types given name on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "Create a new space" modal has disappeared
    Then user of browser sees that the new item has appeared on the spaces list

  Scenario: User successfully creates new space with specified name (clicks CREATE confirmation button after entering space name)
    Given user of browser generates valid name string
    When user of browser clicks on the "Create" button in spaces sidebar
    And user of browser sees that input box in "Create a new space" modal is active
    And user of browser types given name on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that "Create a new space" modal has disappeared
    Then user of browser sees that the new item has appeared on the spaces list

  # 'space1' defined in env.json
  Scenario: User successfully renames space (clicks ENTER after entering space name)
    Given that in browser there is a "space1" item on the spaces list
    When user of browser clicks a settings icon displayed for "space1" item on the spaces list
    And user of browser sees a settings dropdown menu for "space1" item on the spaces list
    And user of browser clicks on the "RENAME" item in current settings dropdown
    And user of browser sees that input box in "Rename a space" modal is active
    And user of browser types "NewNameSpace" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    And user of browser sees that "Rename a space" modal has disappeared
    Then user of browser sees that the "space1" has disappeared from the spaces list
    And user of browser sees that the "NewNameSpace" has appeared on the spaces list
    # TODO rm code below after REST API become available
    And user of browser clicks a settings icon displayed for "NewNameSpace" item on the spaces list
    And user of browser sees a settings dropdown menu for "NewNameSpace" item on the spaces list
    And user of browser clicks on the "RENAME" item in current settings dropdown
    And user of browser sees that input box in "Rename a space" modal is active
    And user of browser types "space1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*NewNameSpace.*renamed.*space1.*
    And user of browser sees that "Rename a space" modal has disappeared
    And user of browser sees that the "NewNameSpace" has disappeared from the spaces list
    And user of browser sees that the "space1" has appeared on the spaces list

  # 'space1' defined in env.json
  Scenario: User successfully renames space (clicks OK confirmation button after entering space name)
    Given that in browser there is a "space1" item on the spaces list
    When user of browser clicks a settings icon displayed for "space1" item on the spaces list
    And user of browser sees a settings dropdown menu for "space1" item on the spaces list
    And user of browser clicks on the "RENAME" item in current settings dropdown
    And user of browser sees that input box in "Rename a space" modal is active
    And user of browser types "NewNameSpace" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    And user of browser sees that "Rename a space" modal has disappeared
    Then user of browser sees that the "space1" has disappeared from the spaces list
    And user of browser sees that the "NewNameSpace" has appeared on the spaces list
    # TODO rm code below after REST API become available
    And user of browser clicks a settings icon displayed for "NewNameSpace" item on the spaces list
    And user of browser sees a settings dropdown menu for "NewNameSpace" item on the spaces list
    And user of browser clicks on the "RENAME" item in current settings dropdown
    And user of browser sees that input box in "Rename a space" modal is active
    And user of browser types "space1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: .*NewNameSpace.*renamed.*space1.*
    And user of browser sees that "Rename a space" modal has disappeared
    And user of browser sees that the "NewNameSpace" has disappeared from the spaces list
    And user of browser sees that the "space1" has appeared on the spaces list

  # 'space1' defined in env.json
  Scenario: Check if "invite group" token box is not empty
    Given that in browser there is a "space1" item on the spaces list
    When user of browser clicks a settings icon displayed for "space1" item on the spaces list
    And user of browser sees a settings dropdown menu for "space1" item on the spaces list
    And user of browser clicks on the "INVITE GROUP" item in current settings dropdown
    And user of browser sees that token box in "Invite group to the space" modal is active
    Then user of browser sees non-empty token in active modal

  # 'space1' defined in env.json
  Scenario: Check if "get support" token box is not empty
    Given that in browser there is a "space1" item on the spaces list
    When user of browser clicks a settings icon displayed for "space1" item on the spaces list
    And user of browser sees a settings dropdown menu for "space1" item on the spaces list
    And user of browser clicks on the "GET SUPPORT" item in current settings dropdown
    And user of browser sees that token box in "Get support for the space" modal is active
    Then user of browser sees non-empty token in active modal

  # 'space1' defined in env.json
  Scenario: User fails to join to space because of using invalid token (clicks ENTER after entering token)
    Given that in browser there is a "space1" item on the spaces list
    When user of browser clicks on the "Join" button in spaces sidebar
    And user of browser sees that input box in "Join a space" modal is active
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees an error notify with text matching to: .*[Ii]nvalid.*token.*

  # 'space1' defined in env.json
  Scenario: User fails to join to space because of using invalid token (clicks Join confirmation butto after entering token)
    Given that in browser there is a "space1" item on the spaces list
    When user of browser clicks on the "Join" button in spaces sidebar
    And user of browser sees that input box in "Join a space" modal is active
    And user of browser types "helloworld" on keyboard
    And user of browser clicks "Join" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*[Ii]nvalid.*token.*

  # 'space2' defined in env.json
  Scenario: Switching between spaces
    When user of browser can see current url
    And user of browser selects "space2" from spaces list
    Then user of browser sees that submenu for space named "space2" has appeared
    And user of browser sees that url has changed

  # 'space1' and 'space2' defined in env.json
  Scenario: Set given space as home and than set previous space as home
    Given that in browser there is a "space2" item on the spaces list
    When user of browser clicks a settings icon displayed for "space2" item on the spaces list
    And user of browser sees a settings dropdown menu for "space2" item on the spaces list
    And user of browser clicks on the "SET AS HOME" item in current settings dropdown
    Then user of browser sees an info notify with text matching to: .*space2.*home.*
    And user of browser sees that home space icon has appeared next to displayed name of space "space2" in spaces list
    # TODO rm code below after REST API become available
    And user of browser clicks a settings icon displayed for "space1" item on the spaces list
    And user of browser sees a settings dropdown menu for "space1" item on the spaces list
    And user of browser clicks on the "SET AS HOME" item in current settings dropdown
    And user of browser sees an info notify with text matching to: .*space1.*home.*
    And user of browser sees that home space icon has appeared next to displayed name of space "space1" in spaces list

  # 'space2' defined in env.json
  Scenario: Leave existing space and then create space with the same name
    Given that in browser there is a "space2" item on the spaces list
    When user of browser clicks a settings icon displayed for "space2" item on the spaces list
    And user of browser sees a settings dropdown menu for "space2" item on the spaces list
    And user of browser clicks on the "LEAVE SPACE" item in current settings dropdown
    And user of browser clicks "YES" confirmation button in displayed modal
    And user of browser sees that "Leave a space" modal has disappeared
    Then user of browser sees an info notify with text matching to: .*space2.*left
    And user of browser sees that the "space2" has disappeared from the spaces list
    # TODO rm code below after REST API become available
    And user of browser clicks on the "Create" button in spaces sidebar
    And user of browser sees that input box in "Create a new space" modal is active
    And user of browser types "space2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "Create a new space" modal has disappeared
    And user of browser sees that the "space2" has appeared on the spaces list
