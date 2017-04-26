Feature: Deployment process using panel of zone and provider


  Background:
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [z1 zone panel, p1 provider panel] page
    And users of [browser1, browser2] entered credentials for [admin, admin] in login form
    And users of [browser1, browser2] pressed Sign in button

    And users of [browser1, browser2] clicked on Create new cluster button in content page for "New cluster" sidebar record


  Scenario: User deploy cluster
    When user of browser1 checks [Database, Cluster Worker, Cluster Manager, Primary Cluster Manager] options for .*onezone.* host
    And user of browser1 clicks on Deploy button in ...
