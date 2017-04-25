Feature: Deployment process using panel of zone and provider


  Background:
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [z1 zone panel, p1 provider panel] page
    And users of [browser1, browser2] entered credentials for [admin, admin] in login form
    And users of [browser1, browser2] pressed Sign in button


  Scenario: User deploy cluster
    When user of browser uses spaces select to change data space to "space4"
