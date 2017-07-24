Feature: Basic spaces management utilities using onepanel

  Background:
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [p1 provider panel, z1 onezone] page
    And user of browser1 logged as admin to Onepanel service
    And user of browser2 seen Z1 zone name in oz login page
    And user of browser2 logged as admin to Onezone service


  Scenario: Support space
    # create space
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 types "helloworld" to space creation edit box in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 presses enter on keyboard
    And user of browser2 sees that space named "helloworld" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel

    # receive support token
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "ADD STORAGE" item in settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that modal "Add storage" has appeared
    And user of browser2 sees non-empty token in "Add storage" modal
    And user of browser2 copies token from "Add storage" modal
    And user of browser2 sends copied token to user of browser1

    # support space
    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on Support space button in spaces page in Onepanel if there are some spaces already supported
    And user of browser1 selects "onestorage" from storage selector in support space form in onepanel
    And user of browser1 types received token to Support token field in support space form in Onepanel
    And user of browser1 types "1" to Size input field in support space form in Onepanel
    And user of browser1 selects GB radio button in support space form in Onepanel
    And user of browser1 clicks on Support space button in support space form in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Aa]dded.*support.*space.*
    And user of browser1 sees that space support record for "helloworld" has appeared in Spaces page in Onepanel

    # confirm support of space
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser2 sees that list of supporting providers for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel contains only: "p1"


  Scenario: Revoke space support
    # assert space existence and support
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that list of supporting providers for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel contains only: "p1"

    # unsupport space
    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 expands toolbar for "helloworld" space record in Spaces page in Onepanel
    And user of browser1 clicks on Revoke space support option in space's toolbar in Onepanel
    And user of browser1 clicks on Yes, revoke button in REVOKE SPACE SUPPORT modal in Onepanel
    And user of browser1 sees an info notify with text matching to: .*[Ss]upport.*revoked.*

    # confirm lack of support for space
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser2 sees that there is/are no supporting provider(s) named "p1" for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
