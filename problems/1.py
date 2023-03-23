import PyPDF2

# Open the PDF file in read-binary mode
with open('example.pdf', 'rb') as pdf_file:
    # Create a PDF reader object
    pdf_reader = PyPDF2.PdfReader(pdf_file)

    # Initialize an empty string to store the text
    text = ''

    # Loop through each page in the PDF file
    for page_num in range(len(pdf_reader.pages)):
        # Get the page object
        page = pdf_reader.pages[page_num]

        # Extract the text from the page
        page_text = page.extract_text()

        # Append the page text to the overall text string
        text += page_text

# Open a new text file in write mode
with open('example.txt', 'w') as text_file:
    # Write the text string to the text file
    text_file.write(text)
