<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="html"  doctype-system="about:legacy-compat" />
	<xsl:template match="/IntegratedUnitTest">
	<html>
		<head>
			<title>Integrated Unit Test - Output</title>
			<link rel="stylesheet" type="text/css" href="output.css"/>
			<script type="text/javascript" src="jquery.js"></script>
			<script type="text/javascript" src="output.js"></script>
		</head>
		<body>
			<div id="container">
				<div class='fleft'>
					<h1>Integrated Unit Test</h1>
				</div>
				<div class='info'>
					<div><xsl:value-of select="info/file" /></div>
					<div><xsl:value-of select="info/time" /></div>
				</div>
				<div class="clear"></div>
				<xsl:for-each select="class">
				<div class="class">
					<div><span class='label'>Class: </span><xsl:value-of select="@name" /></div>
					<xsl:for-each select='function'>
					<div class='function'>
						<div class='function-header'>
							<div class='fleft'>
								<span class='label'>Function: </span><xsl:value-of select="@name" />
								<span class="show-tests expand-button"><span>[+]</span><span class="min">[&#8211;]</span></span>
							</div>
							<div class='fright'>
								<span class="function-result"></span>
								<span class='label'><xsl:value-of select="@type" /></span>
							</div>
							<div class="clear"></div>
						</div>
						<div class="tests">
						<xsl:for-each select="tests/test">
							<div>
								<xsl:attribute name="class">
									<xsl:text>test </xsl:text>
									<xsl:value-of select="@result" />
								</xsl:attribute>
								<div class="fleft">
									<xsl:value-of select="@displayname" /> (
									<xsl:for-each select="params/param">
										<span class='label'><xsl:value-of select="@name" />: </span>
										<xsl:value-of select="." />
										<xsl:text>; </xsl:text>
									</xsl:for-each> )
									<xsl:if test="equals">
										<xsl:choose>
											<xsl:when test="equals/@* = 'binary.base64'"><div class="eqval b64"><xsl:value-of select="equals" /></div></xsl:when>
											<xsl:otherwise><div class="eqval"><xsl:value-of select="equals" /></div></xsl:otherwise>
										</xsl:choose>
									</xsl:if>
								</div>
								<div class="fright">
									<span class='label'>Returned: </span><xsl:value-of select="@result" />
								</div>
								<div class="clear"></div>
							</div>
							<xsl:if test="implies">
								<xsl:for-each select="implies/eval">
									<div class="implies">
										<span>
											<xsl:attribute name="class">
												<xsl:text>implies-result </xsl:text>
												<xsl:value-of select="@result" />
											</xsl:attribute>
										</span>
										<span class="implies-content"><xsl:value-of select="." /></span>
									</div>
								</xsl:for-each>
							</xsl:if>
						</xsl:for-each>
						</div>
					</div>
					</xsl:for-each>
				</div>
				</xsl:for-each>
			</div>
		</body>
	</html>
	</xsl:template>
</xsl:stylesheet>