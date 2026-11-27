open Expln_test

describe("MM_mcp_client.isPathAllowed", _ => {
    it("allows the API functions used by the MCP server", _ => {
        assertEq(MM_mcp_client.isPathAllowed("tabs.list"), true)
        assertEq(MM_mcp_client.isPathAllowed("editor.getState"), true)
        assertEq(MM_mcp_client.isPathAllowed("explorer.findAssertions"), true)
        assertEq(MM_mcp_client.isPathAllowed("context.load"), true)
    })

    it("rejects other API functions and malformed paths", _ => {
        assertEq(MM_mcp_client.isPathAllowed("macro.runMacro"), false)
        assertEq(MM_mcp_client.isPathAllowed("settings.getSettings"), false)
        assertEq(MM_mcp_client.isPathAllowed("editor.proveBottomUp"), false)
        assertEq(MM_mcp_client.isPathAllowed("editor"), false)
        assertEq(MM_mcp_client.isPathAllowed("tabs.list.call"), false)
        assertEq(MM_mcp_client.isPathAllowed("editor.getState.constructor"), false)
        assertEq(MM_mcp_client.isPathAllowed(" tabs.list"), false)
        assertEq(MM_mcp_client.isPathAllowed(""), false)
    })
})
